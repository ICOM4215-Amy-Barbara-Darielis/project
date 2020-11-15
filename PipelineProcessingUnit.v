
// Course: ICOM4215
// Project Name: Processing Pipeline Unit
// Contributors:
//				Amy Ayala
// 				Barbara Gonzalez-Rivera
// 				Darielis Morales Rodríguez

/**********************************************************
 *                    Control Unit                        *
 **********************************************************/
module ControlUnit(output reg [1:0] Data_Mem_Opcode, output reg [3:0] alu_Op, output reg B_Instr,  Shift_imm,  Load_instr,  RF_enable, ID_S, Br_L_Instr, input [31:0] I, input reset);
  // Control unit decodes instructions and provides appropiate control signals.
  reg invalid; //This bit will be set when invalid instructions are received
  always @(I, posedge reset)
    begin
    	invalid = 0;
    if(reset)
      begin
        Data_Mem_Opcode = 2'b0;
        alu_Op = 3'b0;
        B_Instr = 1'b0;
        Shift_imm = 1'b0;
        Load_instr= 1'b0;
        RF_enable= 1'b0;
        ID_S = 1'b0;
        Br_L_Instr = 1'b0;
      end
    else
     begin 
        alu_Op = I[24:21];
        Data_Mem_Opcode = 2'b00;
        Load_instr = 0;
        Shift_imm = 0;
        B_Instr = 0;
        RF_enable = 0;
       	ID_S = I[20];
        Br_L_Instr = 1'b0;
        case (I[27:25])
        3'b000: 
          begin 
            //Check for invalid instructions
            if( I[4] == 1 || (I[20] == 0 && I[24:23] == 2'b10) || (I[11:7] == 5'b00000 && (I[6:5] == 2'b00 || I[6:5] == 2'b11)) ) //Last OR(AND(OR) operation to filter invalid shifter operands(data processing instructions)
              	begin
                	invalid = 1;
                end
            Data_Mem_Opcode = 2'b10;
            Shift_imm = 1;
            RF_enable = 1;
          end
        3'b001: 
          begin
            //Check for invalid instructions
            if( I[24:23] == 2'b10 && (I[21:20] == 2'b00 || I[21:20] == 2'b10) )
            	begin
                	invalid = 1;
            	end
            Data_Mem_Opcode = 2'b10;
            RF_enable = 1;
          end
        3'b010:            //no invalid instructions
          begin
            Load_instr= I[20];
            ID_S = 0;
            if(Load_instr == 0) 
                begin
                if(I[22] == 0) //b==0
                     Data_Mem_Opcode = 2'b10;
                  else 
                     Data_Mem_Opcode = 2'b00;
                end
            else 
                begin
                alu_Op = 4'b0100;
                RF_enable = 1;
                if(I[22] == 0) //b==0
                     Data_Mem_Opcode = 2'b10;
                else 
                     Data_Mem_Opcode = 2'b00;
                end
          end
        3'b011:
          begin
             ID_S = 0;
            //Check for invalid instructions
            if(I[4]==1)
            	begin
                	invalid=1;
            	end
            
            Load_instr= I[20];
            if(Load_instr == 0) 
              begin
                if(I[22] == 0) //b==0
                     Data_Mem_Opcode = 2'b10;
                else 
                     Data_Mem_Opcode = 2'b00;
              end 
            else 
              begin
                alu_Op = 4'b0100;
                RF_enable = 1;
                if(I[22] == 0) //b==0 
                     Data_Mem_Opcode = 2'b10;
                  else 
                     Data_Mem_Opcode = 2'b00;
              end
          end
        3'b101:             //no invalid instructions
          begin
            ID_S = 0;
            if(I[24] == 0) 
              begin
                  Data_Mem_Opcode = 2'b10;
                  B_Instr = 1;
              end 
            else 
              begin
                  Data_Mem_Opcode = 2'b10;
                  B_Instr = 1;
                  RF_enable = 1;
                  Br_L_Instr = 1;
              end
          end
        default:
            //Check for invalid instructions that do not match any other format
            begin
            	invalid = 1;
            end
        endcase
        //Check for invalid instructions
        if(I[31:28] == 4'b1111)
        	begin
          		invalid = 1;
        	end
       if(invalid)
         begin
           //$display("invalid");
           Data_Mem_Opcode = 2'b00;
           alu_Op = 3'b000;
           B_Instr = 0;
           Shift_imm = 0;
           Load_instr = 1;
           RF_enable = 0;
           ID_S=0;
           Br_L_Instr = 1'b0;
         end
     end
     end 
endmodule

/**********************************************************
 *                  mux_8x4_32b                          *
 **********************************************************/
module mux_8x4_32b(output reg Y_0,output reg [4:1] Y_1_4, output reg Y_5, Y_6, input S, A_0, input[4:1] A_1_4,input A_5, A_6, B_0,input[4:1] B_1_4,input B_5, B_6);
    // This multiplexer chooses between two sets of 4 inputs.
  // In our application it send zeroes for the control lines, instead of the the control unit output when there is control hazards.
  always @ (*)    
      case (S)
        1'b0: 
          begin 
             
          Y_0 = A_0;
          Y_1_4 = A_1_4;
          Y_5 = A_5;
          Y_6 = A_6;
          end
        1'b1: 
          begin 
             
          Y_0 = B_0;
          Y_1_4 = B_1_4;
          Y_5 = B_5;
          Y_6 = B_6;
          end 
      endcase
endmodule


/**********************************************************
 *              Hazards/Forwarding Unit                  *
 **********************************************************/
module Hazards_Forwarding(output reg [1:0] ForwardA, ForwardB, output reg IF_ID_LE, PCLE, no_op_mux, 
    input [3:0] ID_Rm, ID_Rn, EX_Rd, MEM_Rd, WB_Rd, input EX_RF_enable, WB_RF_enable, MEM_RF_enable, EX_load_instr);
   always @ (ID_Rm, ID_Rn, EX_Rd, MEM_Rd, WB_Rd, EX_RF_enable, WB_RF_enable, MEM_RF_enable, EX_load_instr)
    begin
        //Initially no data hazard has been detected yet, standard register contents are passed in ID stage
        ForwardA <= 2'b11; 
        ForwardB <= 2'b00;
      
        //Will choose nearest stage to ID when Data hazard occurs
        //WB Forwarding
      if(WB_RF_enable)
            begin
                if(ID_Rm == WB_Rd) ForwardA <= 2'b00; //First connection of mux A is WB_Rd
                if (ID_Rn == WB_Rd) ForwardB <= 2'b11; //Fourth connection of mux B is WB_Rd
            end
            
        //MEM Forwarding
        if(MEM_RF_enable)
            begin
                if(ID_Rm == MEM_Rd) ForwardA <= 2'b01; //Second connection of mux A is MEM_Rd
                if (ID_Rn == MEM_Rd) ForwardB <= 2'b10 ; //Third connection of mux B is MEM_Rd
            end
            
        //EX Forwarding
       if(EX_RF_enable) 
           begin
                if(ID_Rm == EX_Rd) ForwardA <= 2'b10; //Third connection of mux B is Ex_Rd
                if (ID_Rn == EX_Rd) ForwardB <= 2'b01; //Second connection of mux B is Ex_Rd
            end

        //Initially, no data hazard by load instruction, sends control to CU mux
        PCLE <= 1; 
        IF_ID_LE <= 1; 
        no_op_mux <= 0; 
        
        //Data hazard by load instruction, controlling CU mux
        // no_op_mux = 0 means control unit signals are sent instead of no op
        if(EX_load_instr && ID_Rm == EX_Rd) 
            begin
                PCLE <= 0; 
                IF_ID_LE <= 0; 
                no_op_mux <= 1; 
            end
        if(EX_load_instr && ID_Rn == EX_Rd) 
            begin
                PCLE <= 0; 
                IF_ID_LE <= 0; 
                no_op_mux <= 1; 
            end
    end
endmodule  

/**********************************************************
 *                 Condition Handler                      *
 **********************************************************/
module conditionhandler(output reg cond_output, Br_L_asserted, input condN,condZ,condC,condV, ID_B_instr, ID_Br_L_Instr, input [3:0] CC);
  // Checks condition is met and returns cond_output= 1 if branch instruction indicator is asserted (1)
  // and conditional output according to necessary conditional code is met.
  reg cond_true;
  always @ (*)
        begin
            cond_true = 0;
            case(CC)
                 4'b0000: cond_true = condZ;
                 4'b0001: cond_true = !condZ;
                 4'b0010: cond_true = condC;
                 4'b0011: cond_true = !condC;
                 4'b0100: cond_true = condN;
                 4'b0101: cond_true = !condN;
                 4'b0110: cond_true = condV;
                 4'b0111: cond_true = !condV;
                 4'b1000: cond_true = (condC && !condZ);
                 4'b1001: cond_true = (!condC || condZ);
                 4'b1010: cond_true = (condN == condV);
                 4'b1011: cond_true = (condN != condV);
                 4'b1100: cond_true = ((condN == condV) && !condZ);
                 4'b1101: cond_true = ((condN != condV) || !condZ);
                 4'b1110: cond_true = 1;
            endcase
          if(cond_true && ID_B_instr) 
            cond_output = 1;
          else 
            cond_output = 0;
          
          if(cond_true && ID_Br_L_Instr)
            Br_L_asserted = 1;
          else
            Br_L_asserted = 0;
          $display("%b, %b, %b", cond_true, CC, cond_output );
        end
endmodule


/**********************************************************
 *              Current Program Status Register           *
 **********************************************************/
module CPSR(output reg oN,oZ,oC,oV,input iS,iN,iC,iZ,iV);
  reg N=0;
  reg Z=0;
  reg C=0;
  reg V=0;
  assign oN = N;
  assign oZ = Z;
  assign oC = C;
  assign oV = V;
  always @ (*)
    if(iS)
      begin
        N = iN;
        Z = iZ;
        C = iC;
        V = iV;
      end
endmodule

/**
module CPSRtest;
    reg iS,iN,iC,iZ,iV;
    wire oN,oZ,oC,oV;
    CPSR test(oN,oZ,oC,oV,iS,iN,iC,iZ,iV);

  initial #100 $finish;
  
  initial fork
    iN = 1'b0; 
    iZ = 1'b1;
    iC = 1'b1; 
    iV = 1'b0;
    iS = 1'b0;
    #1 iS = 1'b1;
  join
  initial begin
    $display ("SR: N Z C V S  Nf Zf Cf Vf           Time:");
    $monitor ("    %b %b %b %b %b  %b  %b  %b  %b  %d ", iN, iZ, iC, iV, iS, oN, oZ, oC, oV,  $time); 
  
  end

//end of ALUtest
endmodule
**/
 
/**********************************************************
 *                         ALU                            *
 **********************************************************/

module ALU(output reg[31:0] result, output reg condN,condZ,condC,condV, input[31:0]in1,in2, input[3:0]opCode, input carry);
 //Used to help check to see if there is a carry
  reg[32:0] isThereACarry;
   always @ (*) 
  	begin
      //we use a case and opcode as input to understand which operation to execute
      //The following order for the opcode is taken from the Data-Processsing Instructions slide 
      //that was given in the class
      case(opCode)
      
            /*********
            Data Processing Instructions
            ***************/
            
            //1. 0000 AND 
                4'b0000:
        			begin
            			//Rd = Rn AND shifter_operand
            			result = (in1 & in2);
                        //else if S == 1 then
                                //N Flag = Rd[31]
                                condN = result[31];
                                //Z Flag = if Rd == 0 then 1 else 0
                                if(result==0)
                    					condZ = 1;
                    			else
                    					condZ = 0;
                    			//C Flag = shifter_carry_out
                    			condC = carry;
                    			//V Flag = unaffected
        			end //end of AND
            
            //2. 0001 EOR 
                4'b0001: 
        			begin
        			    //Rd = Rn EOR shifter_operand
        			    result = in1 ^ in2;
                                //N Flag = Rd[31]
                                condN = result[31];
                                //Z Flag = if Rd == 0 then 1 else 0
                                if(result==0)
                    					condZ = 1;
                    			else
                    					condZ = 0;
                    		    //C Flag = CarryFrom(Rn + shifter_operand + C Flag)
                                condC = isThereACarry[32];
                                //V Flag = unaffected
        			end //end of EOR
            
            //3. 0010 SUB 
                4'b0010: 
                			begin
                    			// Rd = shifter_operand - Rn
                				isThereACarry = in1 - in2;
                				result = in1 - in2;
                                        //N Flag = Rd[31]
                                        condN = result[31];
                                        //Z Flag = if Rd == 0 then 1 else 0
                                        if(result==0)
                            					condZ = 1;
                            			else
                            					condZ = 0;
                                        //C Flag = CarryFrom(Rn + shifter_operand + C Flag)
                                        condC = isThereACarry[32];
                                        //V Flag = OverflowFrom(Rn + shifter_operand + C Flag)
                                        condV= (in1[31] & !in2[31] & !result[31]) | (!in1[31] & in2[31] & result[31]);
                			end //end of SBC
            
            //4. 0011 RSB 
                4'b0011: 
    			    begin
    			        //Rd = shifter_operand - Rn
        				isThereACarry = in2 - in1;
        				result = in2 - in1;
                                //N Flag = Rd[31]
                                condN = result[31];
                                //Z Flag = if Rd == 0 then 1 else 0
                                if(result==0)
                    					condZ = 1;
                    			else
                    					condZ = 0;
                    		    //C Flag = CarryFrom(Rn + shifter_operand + C Flag)
                                condC = isThereACarry[32];
                                //V Flag = OverflowFrom(Rn + shifter_operand + C Flag)
                                condV=	(!in1[31] & in2[31] & !result[31]) | (in1[31] & !in2[31] & result[31]);
    		    	end //end of RSB
    		    	
            //5. 0100 ADD 
		        4'b0100: 
		            begin
                        //Rd = Rn + shifter_operand
                        isThereACarry = in1 + in2;
			            result = in1 + in2;
                                //N Flag = Rd[31]
                                condN = result[31];
                                //Z Flag = if Rd == 0 then 1 else 0
                                if(result==0)
                    					condZ = 1;
                    			else
                    					condZ = 0;
                    		    //C Flag = CarryFrom(Rn + shifter_operand + C Flag)
                                condC = isThereACarry[32];
                                //V Flag = OverflowFrom(Rn + shifter_operand + C Flag)
                                condV = (in1[31] & in2[31] & !result[31]) | (!in1[31] & !in2[31] & result[31]);
		            end //end of ADD

            //6. 0101 ADC 
                4'b0101:
                    begin
                        //Rd = Rn + shifter_operand + C Flag
                        isThereACarry = in1 + in2 + carry;
			            result = in1 + in2 + carry;
                                //N Flag = Rd[31]
                                condN = result[31];
                                //Z Flag = if Rd == 0 then 1 else 0
                                if(result==0)
                    					condZ = 1;
                    			else
                    					condZ = 0;
                                //C Flag = CarryFrom(Rn + shifter_operand + C Flag)
                                condC = isThereACarry[32];
                                //V Flag = OverflowFrom(Rn + shifter_operand + C Flag)
                                condV = (in1[31] & in2[31] & !result[31]) | (!in1[31] & !in2[31] & result[31]);
                    end //end of ADC
                    
            //7. 0110 SBC 
                    4'b0110: 
            			begin
                			//using the book as a reference
                			// Rd = shifter_operand - Rn - NOT(C Flag)
            				isThereACarry = in1 - in2 - !(carry);
            				result = in1 - in2 - !(carry);
                                    //N Flag = Rd[31]
                                    condN = result[31];
                                    //Z Flag = if Rd == 0 then 1 else 0
                                    if(result==0)
                        					condZ = 1;
                        			else
                        					condZ = 0;
                                    //C Flag = CarryFrom(Rn + shifter_operand + C Flag)
                                    condC = isThereACarry[32];
                                    //V Flag = OverflowFrom(Rn + shifter_operand + C Flag)
                                    condV= (in1[31] & !in2[31] & !result[31]) | (!in1[31] & in2[31] & result[31]);
            			end //end of SBC
            
            //8. 0111 RSC 
                4'b0111: 
        			    begin
            				isThereACarry = in2 - in1 - !(carry);
				            result = in2 - in1 - !(carry);
                                    //N Flag = Rd[31]
                                    condN = result[31];
                                    //Z Flag = if Rd == 0 then 1 else 0
                                    if(result==0)
                        					condZ = 1;
                        			else
                        					condZ = 0;
                        		    //C Flag = CarryFrom(Rn + shifter_operand + C Flag)
                                    condC = isThereACarry[32];
                                    //V Flag = OverflowFrom(Rn + shifter_operand + C Flag)
                                    condV=	(!in1[31] & in2[31] & !result[31]) | (in1[31] & !in2[31] & result[31]);
        		    	end //end of RSB
            
            //9. 1000 TST 
            4'b1000: 
    			begin
    				result = in1 & in2;
    				condC = carry;
    				condN = result[31];
    				if(result==0)
    				    condZ=1;
    				else
    				    condZ=0;
    			//flag v is unnefected
    			end
            
            //10. 1001 TEQ 
                4'b1001: 
        			begin
        		    //alu_out = Rn EOR shifter_operand
        				result = in1 ^ in2;
        				condC = carry;
        				condN = result[31];
        				if(result==0)
        				    condZ=1;
        				else
        				    condZ=0;
        			//flag v is unnefected
        			end
            
            //11. 1010 CMP 
                4'b1010: 
            			begin
                			//alu_out = Rn - shifter_operand
                			isThereACarry = in1 - in2;
            				result = in1 - in2;
                            //N Flag = alu_out[31]
                            condN = result[31];
                            //Z Flag = if Rd == 0 then 1 else 0
                                    if(result==0)
                        					condZ = 1;
                        			else
                        					condZ = 0;
                            //C Flag = CarryFrom(Rn + shifter_operand)
                            condC = isThereACarry[32];
                            //V Flag = OverflowFrom(Rn + shifter_operand)
                            condV= (in1[31] & !in2[31] & !result[31]) | (!in1[31] & in2[31] & result[31]);
    			        end //end of CMP
            
            //12. 1011 CMN 
                4'b1011: 
        			begin
            			//alu_out = Rn + shifter_operand
            			isThereACarry = in1 + in2;
        				result = in1 + in2;
                        //N Flag = alu_out[31]
                        condN = result[31];
                        //Z Flag = if Rd == 0 then 1 else 0
                                if(result==0)
                    					condZ = 1;
                    			else
                    					condZ = 0;
                        //C Flag = CarryFrom(Rn + shifter_operand)
                        condC = isThereACarry[32];
                        //V Flag = OverflowFrom(Rn + shifter_operand)
                           condV = (in1[31] & in2[31] & !result[31]) | (!in1[31] & !in2[31] & result[31]);
			        end //end of CMN
			        
            //13. 1100 ORR
                4'b1100: 
                    begin
                        //Rd = Rn OR shifter_operand
    			    	result = in1 | in2;
                                //N Flag = Rd[31]
                                condN = result[31];
                                //Z Flag = if Rd == 0 then 1 else 0
                                if(result==0)
                    					condZ = 1;
                    			else
                    					condZ = 0;
                    			//C Flag = shifter_carry_out
                    			condC = carry;
                    			//V Flag = unaffected
                    end //end of ORR

            //14. 1101 MOV
                4'b1101: 
        			begin
        				//Rd = shifter_operand
        				result = in2;
        				       //N Flag = alu_out[31]
                                condN = result[31];
                                //Z Flag = if Rd == 0 then 1 else 0
                                        if(result==0)
                            					condZ = 1;
                            			else
                            					condZ = 0;
                                //C Flag = CarryFrom(Rn + shifter_operand)
                                condC = isThereACarry[32];
                                //V Flag = unaffected
        			end //end MOV
            
            //15. 1110 BIC 	
                4'b1110:
                        begin
                            //Rd = Rn AND NOT shifter_operand
                            result = in1 & ~in2;
                                    //N Flag = Rd[31]
                                    condN = result[31];
                                    //Z Flag = if Rd == 0 then 1 else 0
                                    if(result==0)
                        					condZ = 1;
                        			else
                        					condZ = 0;
                                    //C Flag = shifter_carry_out
                            		condC = carry;
                                    //V Flag = unaffected
                        end //end of BIC
               
            //16. 1111 MVN
            4'b1111: 
        			begin
        				//Rd = shifter_operand
        				result = ~in2;
        				       //N Flag = alu_out[31]
                                condN = result[31];
                                //Z Flag = if Rd == 0 then 1 else 0
                                        if(result==0)
                            					condZ = 1;
                            			else
                            					condZ = 0;
                                //C Flag = CarryFrom(Rn + shifter_operand)
                                condC = isThereACarry[32];
                                //V Flag = unaffected
        			end //end MOV
        	endcase		
    end //end of always begin
    
endmodule

/**********************************************************
 *                   Shifter/Sign Extender                *
 **********************************************************/
module shifterSignExtender(output reg [31:0] result, output reg OutCarry, output reg S, input[31:0] rm, input [11:0] num12, input[2:0] missingInst, input InCarry);
	//Since we are working with movement of stuff we need a temporary register
	//to not loose vital information
	reg [31:0] tempReg;
	 //por usar always como voy a modificar los outputs por obligacion tengo que declarar los outpus tipo reg
	always @ (*) 
	//Data Processing
	//begin 1
  	begin
      //we use a case and missingInst as input to understand which operation to execute
      case(missingInst)
		//Immediate (second source operand = #<Immediate>)
		//This instruction in the manual is said to be 001 
		3'b001:
			begin 
			/**From the ARM manual
			*shifter_operand = immed_8 Rotate_right (rotate_imm * 2)
			*if rorate_imm == 0 then
			*	shifter_carry_out = C Flag
			*else //rotate_imm !=0 
			*	shifter_carry_out = shifter_operand[31]
			*/
			
			//immed_8 are the bits from num12 located from 0-7
				tempReg = 32'd0 + num12[7:0];
				result = {tempReg,tempReg} >> (num12[11:8]*2);
			    if(num12[11:8]==0)
			        begin
			            OutCarry = InCarry;
			        end
			    else
			        begin
			            OutCarry = result[31];
			        end
			end
	    //Shift by immediate (second source operand = <rm> , <shift> # <shift_imm>). Deben mostrar el resultado para los 4 tipos de shifts. 
	    3'b000:
	        begin
	        //verifying that is isn't shifter register but the other one
	            if(num12[4]==1'b0)
	                begin
	                    //LSL - Logical Shift Left 
	                    //A5.1.5 ARM Manual
	                    if(num12[6:5]==2'b00)
	                        begin
	                        //if shift_imm ==0 
	                            //shifter_operand = Rm
	                            //Shifter_carry_out = C Flag
	                       //else
	                            //shifter_operand = Rm Logical_Shift_left shift_imm
	                            //shifter_carry_out = Rm[32 - shift_imm]
	                       if(num12[11:7]==4'b0000)
	                            begin
	                                result = rm;
	                                OutCarry = InCarry;
	                            end
	                       else
	                           begin
	                                result = rm << num12[11:7];
	                                OutCarry = rm[32-num12[11:7]];
	                           end
	                end ///End of LSL
	                     if(num12[6:5]==2'b01)
    	                     begin
            	                     //LSR - Logical Shift Right
            	                    //A5.1.7
            	                   // if shift_imm == 0 then
            	                   if(num12[11:7]==4'b0000)
            	                        begin
                                    //    shifter_operand = 0
            	                            result = 32'b0;
                                     //   shifter_carry_out = Rm[31]
            	                            OutCarry = rm[31];
            	                        end
            	                   else
            	                   //  else /* shift_imm > 0 */
            	                        begin
            	                       //shifter_operand = Rm Logical_Shift_Right shift_imm
            	                       result = rm >> num12[11:7];
                                      //shifter_carry_out = Rm[shift_imm - 1]
                                      OutCarry = rm[num12[11:7]-1];
            	                       end
	                        end //end of LSR
	                   //Arithmetic Shift Right
	                    if(num12[6:5]==2'b10)
    	                     begin
            	              // if shift_imm == 0 then
            	              if(num12[11:7]==4'b0000)
            	                    begin
            	                      //  if Rm[31] == 0 then
            	                      if(rm[31]==0)
            	                            begin
            	                                 // shifter_operand = 0
            	                                result = 32'b0;
            	                                // shifter_carry_out = Rm[31]
            	                                OutCarry = rm[31];
            	                            end
            	                      else /* Rm[31] == 1 */
            	                            begin
            	                            result = 32'hFFFFFFFF;
            	                            OutCarry = rm[31];
            	                            //shifter_operand = 0xFFFFFFFF
                                           /// shifter_carry_out = Rm[31]
            	                            end
            	                    end
                            else /* shift_imm > 0 */
                                begin
                                result = $signed(rm) >>> num12[11:7];
                                OutCarry = rm[num12[11:7]-1];
                                   // shifter_operand = Rm Arithmetic_Shift_Right <shift_imm>
                                   // shifter_carry_out = Rm[shift_imm - 1]
                                end
	                         end //end of b10
	                         
	                        //rotate right
	                    if(num12[6:5]==2'b11)
	                        begin
            	                //if shift_imm == 0 then
            	                if(num12[11:7]==4'b0000)
                	                begin
                	                    result = (InCarry<<31| rm>>1);
                	                    OutCarry = rm[0];
                	                end
                	           else
                	                begin
                	                    result = {rm,rm} >> num12[11:7];
                	                    OutCarry = rm[num12[11:7]-1];
                	                end
                               // See “Data-processing operands - Rotate right with extend” on page A5-17
                               // else /* shift_imm > 0 */
                                //shifter_operand = Rm Rotate_Right shift_imm
                               //shifter_carry_out = Rm[shift_imm - 1]
                             //end of 11
	                        end
	                end //end of num12
	       //end of 000
	        end
	        3'b010:
	            begin   
	                result = 32'd0 + num12[11:0];
	            end
            3'b011:
                begin   
                    result = rm;
                end
		endcase		
    S = result[20];
	//end of begin 1
	end
//shift by immediate (second //since we need to execute a block of codes, aka run more than one expression, we use begin-end
//end of shifterSignExtender
endmodule

/**********************************************************
 *              MODULES OF REGISTER FILE                  *
 **********************************************************/

//                      REGISTER
module DataReg (output reg[31:0] Q, input [31:0] D, input LE, Clk,reset);
/*
 * 32-bit register. Active when Clk goes up. Data is saved if LE == 1.
 */
  always @ (posedge Clk,posedge reset)
    begin
      if (LE) Q <= D;
      if(reset) Q<= 32'b0;
    end
       
endmodule

//                    MULTIPLEXER 16x1
module mux_16x1 (output reg [31:0] Y, input[3:0] S, input [31:0] a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p);
    always @ (S, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
        case (S)
            4'b0000: Y = a;
            4'b0001: Y = b;
            4'b0010: Y = c;
            4'b0011: Y = d;
            4'b0100: Y = e;
            4'b0101: Y = f;
            4'b0110: Y = g;
            4'b0111: Y = h;
            4'b1000: Y = i;
            4'b1001: Y = j;
            4'b1010: Y = k;
            4'b1011: Y = l;
            4'b1100: Y = m;
            4'b1101: Y = n;
            4'b1110: Y = o;
            4'b1111: Y = p;
        endcase
endmodule

//                  32-BIT MULTIPLEXER 2x1
module mux_2x1_32b (output reg [31:0] Y, input S, input [31:0] A, B);
    // If S=1, Y=B. If S=0, Y=A.
    always @(S, A, B)
        if (S) Y = B;
        else Y = A;
endmodule

//                  1-BIT MULTIPLEXER 2x1
module mux_2x1_1b (output reg Y, input S, input A, B);
    // If S=1, Y=B. If S=0, Y=A.
    always @(S, A, B)
        if (S) Y = B;
        else Y = A;
endmodule

//                  BINARY DECODER
module binary_decoder (output reg [15:0] E, input[3:0] C, input Ld);
    always @ (C, Ld)
    if (Ld)
        case (C)
            4'b0000: E <= 16'b0000000000000001;
            4'b0001: E <= 16'b0000000000000010;
            4'b0010: E <= 16'b0000000000000100;
            4'b0011: E <= 16'b0000000000001000;
            4'b0100: E <= 16'b0000000000010000;
            4'b0101: E <= 16'b0000000000100000;
            4'b0110: E <= 16'b0000000001000000;
            4'b0111: E <= 16'b0000000010000000;
            4'b1000: E <= 16'b0000000100000000;
            4'b1001: E <= 16'b0000001000000000;
            4'b1010: E <= 16'b0000010000000000;
            4'b1011: E <= 16'b0000100000000000;
            4'b1100: E <= 16'b0001000000000000;
            4'b1101: E <= 16'b0010000000000000;
            4'b1110: E <= 16'b0100000000000000;
            4'b1111: E <= 16'b1000000000000000;
        endcase
    else E <= 16'b0000000000000000;
endmodule

//                  REGISTER FILE
module register_file (output reg [31:0] PA, PB, PC, PCout, input [31:0] PD, PCIN, input [3:0] A, B, C, D, input Clk, Ld, PCLE,reset); 
    //PA-PC are three output ports, while PD is one input port.
    //Added extra PCLE and PCIN input for PC counter register R15. Added PC output and third port PC and it's selector C
    wire [15:0] E; //Encoder output to register Ld input, used for register selection during input
    wire [31:0] Qs [15:0]; //16 buses with 32 bits for register outputs
    wire [31:0]tempPCIN;
    wire tempPCLE;
  	reg [31:0] ProgramCounter;
	assign PCout = ProgramCounter;
  	

    // Embedded modules
    binary_decoder binaryDecoder (E, D, Ld); // Decoder
    // Registers
  DataReg R0 (Qs[0], PD, E[0], Clk,reset);
  DataReg R1 (Qs[1], PD, E[1], Clk,reset);
  DataReg R2 (Qs[2], PD, E[2], Clk,reset);
  DataReg R3 (Qs[3], PD, E[3], Clk,reset);
  DataReg R4 (Qs[4], PD, E[4], Clk,reset);
  DataReg R5 (Qs[5], PD, E[5], Clk,reset);
  DataReg R6 (Qs[6], PD, E[6], Clk,reset);
  DataReg R7 (Qs[7], PD, E[7], Clk,reset);
  DataReg R8 (Qs[8], PD, E[8], Clk,reset);
  DataReg R9 (Qs[9], PD, E[9], Clk,reset);
  DataReg R10 (Qs[10], PD, E[10], Clk,reset);
  DataReg R11 (Qs[11], PD, E[11], Clk,reset);
  DataReg R12 (Qs[12], PD, E[12], Clk,reset);
  DataReg R13 (Qs[13], PD, E[13], Clk,reset);
  DataReg R14 (Qs[14], PD, E[14], Clk,reset);
    
    //R15 input through  multiplexers
    mux_2x1_32b muxr15PCIN (tempPCIN, E[15], PCIN, PD);
    mux_2x1_1b muxr15PCLE (tempPCLE, E[15], PCLE, 1'b1);
  DataReg R15 (Qs[15], tempPCIN, tempPCLE, Clk,reset);
    
    // Output of registers in input of multiplexers. Chooses register content to give to output ports
    mux_16x1 muxA (PA, A, Qs[0], Qs[1], Qs[2], Qs[3], Qs[4], Qs[5], Qs[6], Qs[7], Qs[8], Qs[9], Qs[10], Qs[11], Qs[12], Qs[13], Qs[14], Qs[15]);
    mux_16x1 muxB (PB, B, Qs[0], Qs[1], Qs[2], Qs[3], Qs[4], Qs[5], Qs[6], Qs[7], Qs[8], Qs[9], Qs[10], Qs[11], Qs[12], Qs[13], Qs[14], Qs[15]);
    mux_16x1 muxC (PC, C, Qs[0], Qs[1], Qs[2], Qs[3], Qs[4], Qs[5], Qs[6], Qs[7], Qs[8], Qs[9], Qs[10], Qs[11], Qs[12], Qs[13], Qs[14], Qs[15]);
    
    always@(*)
      ProgramCounter = Qs[15];  

endmodule

//                PROGRAM COUNTER ADDER
module adder (output reg [31:0] C, input [31:0] A, B);
  always @ (*)
        C = A + B;
endmodule

/**********************************************************
 *                    MODULE OF DATA MEMORY                *
 **********************************************************/
 module data_ram256x8 (output reg [31:0] DataOut, input Enable, ReadWrite, input [31:0] Address, input [31:0] DataIn, input [1:0] Opcode);

  reg[7:0] Mem[0:255];
  
  always@ (Enable, ReadWrite, Address, DataIn, Opcode)
    if(Enable) begin
      casez (Opcode) 
        2'b00: begin //byte
          if(!ReadWrite) begin
            DataOut = Mem[Address];
        end 
        else begin
          Mem[Address] = DataIn[7:0];
        end
       end
        2'b01: begin //halfword
          if(!ReadWrite) begin
            DataOut = {Mem[Address], Mem[Address+1]};
          end
          else begin
            Mem[Address] = DataIn[15:8];
            Mem[Address+1] = DataIn[7:0];
          end
        end
        2'b10: begin //word
          if(!ReadWrite) begin
            DataOut = {Mem[Address], Mem[Address+1], Mem[Address+2], Mem[Address+3]};
          end
          else begin
            Mem[Address] = DataIn[31:24];
            Mem[Address+1] = DataIn[23:16];
            Mem[Address+2] = DataIn[15:8];
            Mem[Address+3] = DataIn[7:0];
          end
        end
      endcase
    end            
endmodule

/**********************************************************
 *            MODULE OF INSTRUCTIONS MEMORY               *
 **********************************************************/
 module ram256x8(output reg[31:0] DataOut, input Enable, input [31:0] Address, input [31:0] DataIn);
  
reg Read;
assign Read = 1'b0;
reg[7:0] Mem[0:255];

  
   always @ (Enable, Address) 
    begin
      if(!Read) DataOut = {Mem[Address], Mem[Address+1], Mem[Address+2], Mem[Address+3]};
      else 
        Mem[Address] = DataIn;
      end
endmodule

/**********************************************************
 *              PIPELINE PROCESSING UNIT                  *
 **********************************************************/
 
 //          x4(SE) (Shift extends and Multiplies by 4)
module Four_SE (output reg [31:0] out, input [23:0] in);
    reg [23:0] inSE;
    always @ (*)
        begin
            inSE = in;
            out <= inSE*32'd4;
        end
endmodule

//                 PIPELINE REGISTERS
module IFIDRegister (output reg [31:0] I31_0, ID_NextPC, output reg [23:0] I23_0, output reg [11:0] I11_0, output reg [3:0] I3_0, I19_16, I15_12, I31_28, output reg [2:0] I27_25,
                     input [31:0] I, IF_NextPC, input IF_ID_LE, IF_flush, Clk, reset);
    /*
    * Active when Clk goes up. Data is saved if IFID_write == 1.
    */
  always @ (posedge Clk, posedge reset)
    begin
    if(reset)
      begin
       			I31_0  <= 32'b0;
        		ID_NextPC <= 32'b0;
        		I23_0 <= 24'b0;
        		I11_0 <= 12'b0;
        		I3_0 <= 4'b0;
        		I19_16 <= 4'b0;
        		I15_12 <= 4'b0;
        		I31_28 <= 4'b0;
        		I27_25 <= 3'b0; 
      end
    else
      begin
        if (IF_ID_LE) 
            begin
                ID_NextPC <= IF_NextPC;
                if(IF_flush) I31_0 <= 32'b0; 
                else I31_0 <= I;
                I23_0 <= I31_0[23:0];
                I11_0 <= I31_0[11:0];
                I3_0 <= I31_0[3:0];
                I19_16 <= I31_0[19:16];
                I15_12 <= I31_0[15:12];
                I31_28 <= I31_0[31:28];
            end
      end
    end
endmodule

module IDEXRegister (output reg[31:0] EX_PORTm, EX_PORTn, EX_NextPC, output reg [11:0] EX_I11_0, output reg [3:0] EX_I15_12, EX_ALU_op, output reg [2:0] EX_I27_25, output reg [1:0] EX_Data_Mem_Opcode, output reg EX_S, EX_shift_imm, EX_load_instr, EX_RF_enable, EX_Br_L_asserted,
                     input [31:0] ID_Portm, ID_Portn, ID_NextPC, input [11:0] ID_I11_0, input [3:0] ID_I15_12, ID_ALU_op, input [2:0] I27_25, input [1:0] ID_Data_Mem_Opcode, input ID_S, ID_shift_imm, ID_load_instr, ID_RF_enable, Br_L_asserted, Clk,reset); 
  always @ (posedge Clk, posedge reset)
    begin 
     if(reset)
      begin
       		EX_PORTm <=  32'b0;
            EX_PORTn <= 32'b0;
            EX_I11_0 <= 12'b0;
            EX_I15_12 <= 4'b0;
            EX_ALU_op <= 4'b0;
            EX_shift_imm <= 2'b0;
            EX_load_instr <=  2'b0;
            EX_RF_enable <=  2'b0;
            EX_S <=  1'b0;
            EX_I27_25 <= 2'b0;
            EX_Data_Mem_Opcode <= 2'b0;
        	EX_NextPC <= 32'b0;
        	EX_Br_L_asserted <= 32'b0;
      end
    else
         begin
            EX_PORTm <= ID_Portm;
            EX_PORTn <= ID_Portn;
            EX_I11_0 <= ID_I11_0;
            EX_I15_12 <= ID_I15_12;
            EX_ALU_op <= ID_ALU_op;
            EX_shift_imm <= ID_shift_imm;
            EX_load_instr <= ID_load_instr;
            EX_RF_enable <= ID_RF_enable;
            EX_S <= ID_S;
            EX_I27_25 <= I27_25;
            EX_Data_Mem_Opcode <= ID_Data_Mem_Opcode;
            EX_NextPC <= ID_NextPC;
            EX_Br_L_asserted <= Br_L_asserted;
        end
      end
 
endmodule

module EXMEMRegister (output reg[31:0] MEM_PORTn, MEM_ALU_Res,  MEM_NextPC, output reg [3:0] MEM_Cond_Codes, MEM_I15_12, output reg [1:0] MEM_Data_Mem_Opcode, output reg MEM_load_instr, MEM_RF_enable, MEM_Br_L_asserted,
                      input [31:0] EX_PORTn, EX_ALU_Res, EX_NextPC, input [3:0] EX_Cond_Codes, input [3:0] EX_I15_12, input [1:0] EX_Data_Mem_Opcode, input EX_load_instr, EX_RF_enable, EX_Br_L_asserted, Clk,reset); 
  always @ (posedge Clk, posedge reset)
    begin
     if(reset)
       		 begin
             MEM_PORTn <= 32'b0;
       		 MEM_ALU_Res <= 32'b0;
             MEM_NextPC <= 32'b0;
       		 MEM_Cond_Codes <= 4'b0;
       		 MEM_I15_12 = 4'b0;
       		 MEM_load_instr <= 1'b0;
       		 MEM_Data_Mem_Opcode <= 2'b0;
        	 MEM_RF_enable <= 1'b0;
             MEM_Br_L_asserted <=1'b0;
      		end
    else
      begin
        MEM_PORTn <= EX_PORTn;
        MEM_ALU_Res <= EX_ALU_Res;
        MEM_Cond_Codes <= EX_Cond_Codes;
        MEM_I15_12 <= EX_I15_12;
        MEM_load_instr <=EX_load_instr;
        MEM_Data_Mem_Opcode <= EX_Data_Mem_Opcode;
        MEM_RF_enable <= EX_RF_enable;
        MEM_NextPC <= EX_NextPC;
        MEM_Br_L_asserted <= EX_Br_L_asserted;
        end
      end
endmodule

module MEMWBRegister (output reg[31:0] WB_ALU_Res, WB_Data, WB_NextPC, output reg [3:0] WB_I15_12, output reg WB_load_instr, WB_RF_enable, WB_Br_L_asserted,
                      input [31:0] MEM_ALU_Res, Mem_Data, MEM_NextPC, input [3:0] MEM_I15_12, input MEM_load_instr, MEM_RF_enable, MEM_Br_L_asserted, Clk,reset); 
  always @ (posedge Clk,posedge reset)
      begin
        if(reset)
       		begin
            WB_ALU_Res <= 32'b0;
            WB_Data <= 32'b0;
            WB_NextPC <= 32'b0;
            WB_I15_12 <= 4'b0;
            WB_load_instr <= 1'b0;
            WB_RF_enable <= 1'b0;
            WB_Br_L_asserted <= 1'b0;
      		end
    else
      begin
            WB_ALU_Res <= MEM_ALU_Res;
            WB_Data <= Mem_Data;
            WB_I15_12 <= MEM_I15_12;
            WB_load_instr <= MEM_load_instr;
            WB_RF_enable <= MEM_RF_enable;
        	WB_NextPC <= MEM_NextPC;
        	WB_Br_L_asserted <= MEM_Br_L_asserted;
     	 end
      end
endmodule

//                  32-BIT MULTIPLEXER 4x1
module mux_4x1_32b (output reg [31:0] Y, input[1:0] S, input [31:0] a, b, c, d);
    always @ (*)
        case (S)
            2'b00: Y = a;
            2'b01: Y = b;
            2'b10: Y = c;
            2'b11: Y = d;
        endcase
endmodule

//                  32-BIT MULTIPLEXER 3x1
module mux_3x1_32b (output reg [31:0] Y, input S0, S1, input [31:0] a, b, c);
  //Specific multiplexer used for Write-Back stage. Chooses between providing the ouput of the ALU result in a data-processing instruction, the Data Memory in a Load instruction, or the PC+4 for the LINK register (R14). S0 is load bit and S1 is BL asserted bit. a is ALU result, b is memory output, c is PC+4.
    always @ (*)
      begin
      	if(S0)
        	Y = b; //load = 1
      	else if(S1)
    		Y = c; //branch link asserted
  	  	else
        	Y = a; // neither load nor branch link is asserted use ALU result (both are never asserted at the same time)
      end
endmodule

//                  4-BIT MULTIPLEXER 2x1
module mux_2x1_4b(output reg [3:0] Y, input S, input [3:0] A, input [3:0] B);
      always @ (*)
      begin
        if (S) Y = B;
        else Y = A;
      end
endmodule


module Processing_Pipeline_Unit();
    
    //Precharge
    reg [31:0] DataIn = 32'b0;
    integer fi, fo, code, i; reg [31:0] data;
    reg [31:0] Address;
   
    
    // Inputs
    reg [3:0] C;//????
    reg Enable = 1;
    reg Clk;
  //reg [31:0] currentPC = 32'd0;

    
    // Wires
    wire [31:0] DataOut, I31_0;
    wire [31:0] PortC; //????
  wire [31:0] nextPC, currentPC, PCIN, ID_NextPC, EX_NextPC, MEM_NextPC, WB_NextPC, fourSEout, TA;
    wire [31:0] PortA, PortB, ID_PORTm, PortWrite, ID_PORTn, EX_ALU_Res, ALU_in_2, MEM_data_fwd, EX_PORTm, EX_PORTn, SSEresult, MEM_PORTn, MEM_ALU_Res, WB_Data, WB_ALU_Res, Mem_Data;
    wire [23:0] I23_0;
    wire [11:0] I11_0, EX_I11_0;
  wire [3:0] ALU_op, ID_ALU_op, I19_16, I3_0, EX_Rd, MEM_Rd, WB_Rd, EX_ALU_op, I31_28, I15_12, ID_I15_12, EX_Cond_Codes, MEM_Cond_Codes, RF_In_Port;
    wire [2:0] I27_25, EX_I27_25;
    wire [1:0] Data_Mem_Opcode, EX_Data_Mem_Opcode, MEM_Data_Mem_Opcode, ForwardA, ForwardB;
    wire shift_imm, load_instr, RF_enable, ID_B_instr, ID_shift_imm, ID_load_instr, ID_RF_enable, IF_ID_LE, PCLE, no_op_mux, EX_RF_enable, WB_RF_enable, MEM_RF_enable, EX_load_instr, WB_load_instr, EX_shift_imm, MEM_load_instr, EX_S, ID_S, ID_Br_L_Instr, EX_Br_L_Instr, Br_L_asserted, EX_Br_L_asserted, MEM_Br_L_asserted, WB_Br_L_asserted;
    wire oN,oZ,oC,oV, cond_output, condN,condZ,condC,condV, OutCarry;
 
  	reg reset = 1;
    //Outputs

    //Embedded modules
    
  ControlUnit Control_Unit(Data_Mem_Opcode, ALU_op, ID_B_instr, shift_imm, load_instr, RF_enable, ID_S, ID_Br_L_Instr, I31_0,reset);
  
    mux_8x4_32b mux_8x4(ID_shift_imm, ID_ALU_op, ID_load_instr, ID_RF_enable, no_op_mux, shift_imm, ALU_op, load_instr, RF_enable, 1'b0, 4'b0, 1'b0, 1'b0);   
  Hazards_Forwarding HazardForwarding_Unit(ForwardA, ForwardB, IF_ID_LE, PCLE, no_op_mux, I3_0, I19_16, EX_Rd, MEM_Rd, WB_Rd, EX_RF_enable, WB_RF_enable, MEM_RF_enable, EX_load_instr); 
  conditionhandler Condition_Handler(cond_output, Br_L_asserted, oN,oZ,oC,oV, ID_B_instr, ID_Br_L_Instr, I31_28);
    CPSR CPsr(oN,oZ,oC,oV,EX_S,condN,condC,condZ,condV); 

    //Instruction Fetch stage
        //PC comes from Register File R15
  ram256x8 Instruction_Mem(DataOut, Enable, currentPC, DataIn); //Data in belongs to precharge. 
        adder PC_Adder (nextPC, currentPC, 32'd4);
        mux_2x1_32b IF_mux(PCIN, cond_output, nextPC, TA);
        
    //IF/ID transition
        IFIDRegister IFID_Register(I31_0, ID_NextPC, I23_0, I11_0, I3_0, I19_16, I15_12, I31_28, I27_25,  DataOut, nextPC,1'b1, cond_output, Clk,reset); 

    //Instuction Decodification Stage
        register_file Register_File(PortA, PortB, PortC, currentPC, PortWrite, PCIN, I19_16, I3_0, C, RF_In_Port, Clk, WB_load_instr, PCLE,reset); //falta de donde viene C, a donde va POrtC, 
        Four_SE four_SE(fourSEout, I23_0); //4xSE
        adder TA_Adder (TA, ID_NextPC,fourSEout);
        mux_4x1_32b Mux_Rm(ID_PORTm, ForwardA, PortWrite, MEM_data_fwd, EX_ALU_Res, PortA); 
        mux_4x1_32b Mux_Rn(ID_PORTn, ForwardB, PortB, EX_ALU_Res, MEM_data_fwd, PortWrite);
    
    //ID/EX transition
        //I15_12 =  Rd
  IDEXRegister IDEX_Register(EX_PORTm, EX_PORTn, EX_NextPC, EX_I11_0, EX_Rd, EX_ALU_op, EX_I27_25, EX_Data_Mem_Opcode, EX_S, EX_shift_imm, EX_load_instr, EX_RF_enable, EX_Br_L_asserted, ID_PORTm, ID_PORTn, ID_NextPC, I11_0, I15_12, ID_ALU_op, I27_25, Data_Mem_Opcode, ID_S, ID_shift_imm, ID_load_instr, ID_RF_enable, Br_L_asserted, Clk, reset);

    //Execution stage
        mux_2x1_32b EX_mux(ALU_in_2, EX_shift_imm, SSEresult, EX_PORTn); 
        ALU ALU(EX_ALU_Res, condN,condZ,condC,condV, EX_PORTm, ALU_in_2, EX_ALU_op, OutCarry); 
        shifterSignExtender ShifterSign_Extender(SSEresult, OutCarry, EX_S, EX_PORTm, EX_I11_0, EX_I27_25, oC);
    
    //EX/MEM transition
  EXMEMRegister EXMEM_Register(MEM_PORTn, MEM_ALU_Res, MEM_NextPC, MEM_Cond_Codes, MEM_Rd, MEM_Data_Mem_Opcode, MEM_load_instr, MEM_RF_enable, MEM_Br_L_asserted,
                                    EX_PORTn, EX_ALU_Res, EX_NextPC, EX_Cond_Codes, EX_Rd, EX_Data_Mem_Opcode, EX_load_instr, EX_RF_enable, EX_Br_L_asserted, Clk,reset);

    //Memory stage
        data_ram256x8 Data_Mem(Mem_Data, Enable, !MEM_load_instr, MEM_ALU_Res, DataIn, MEM_Data_Mem_Opcode); //Writes when MEM_load_instr==0.
        mux_2x1_32b MEM_mux(MEM_data_fwd, MEM_load_instr, MEM_ALU_Res, Mem_Data);


    //MEM/WB transition
  MEMWBRegister MEM_WB_Register(WB_ALU_Res, WB_Data, WB_NextPC, WB_Rd,  WB_load_instr, WB_RF_enable, WB_Br_L_asserted, MEM_ALU_Res, Mem_Data, MEM_NextPC, MEM_Rd,  MEM_load_instr, MEM_RF_enable, MEM_Br_L_asserted, Clk,reset);

    //Write-Back Stage
  mux_3x1_32b WB_Content_mux(PortWrite, WB_load_instr, WB_Br_L_asserted, WB_ALU_Res, WB_Data, WB_NextPC);
  mux_2x1_4b Link_mux(RF_In_Port, WB_Br_L_asserted, WB_Rd, 4'd14);
   
  //PRECHARGE
  initial begin
  fi = $fopen("input_file.txt","r");
  Address = 32'b00000000000000000000000000000000;
  while(!$feof(fi)) begin
    code = $fscanf(fi,"%b", data);
    Instruction_Mem.Mem[Address] = data; 
   // $display("IR: Address = %d, DataOut = %b", Address, Instruction_Mem.Mem[Address]);  
    Address = Address + 1;
  end
  $fclose(fi);
end  

initial #200 $finish;
  initial begin
    Clk = 0;
    forever #5 Clk = !Clk;
  end
  initial begin
   
    #1   Register_File.ProgramCounter = 32'd0;	
    #1   reset = 0;
   
  end

        initial begin
                // imprimir PC (en decimal) y las señales de control en las etapas ID, EX, MEM y WB (en binario).
          //$display("\nProgram C.                                           ID Control Signals                                                                                               EX Control Signals                                                  MEM Control Signals                    WB Control Signals");
          
         /* $display("    PC    |             I                  |            I31_0               |ID_ALU_op|Data_Mem_Opcode|ID_shift_imm|ID_load_instr|ID_RF_enable|ID_B_instr|ForwardA|ForwardB|EX_ALU_op|EX_shift_imm|EX_load_instr|EX_RF_enable|cond_output|EX_Data_Mem_Opcode|MEM_load_instr|MEM_RF_enable|MEM_Data_Mem_Opcode|WB_load_instr|WB_RF_enable|          MEM_ALU_Res                  |cond_output|  Clk| Time "); 
          $monitor("%d|%b|%b|   %b  |     %b        |      %b     |      %b      |      %b     |     %b    |  %b    |  %b    |  %b   |    %b       |      %b      |      %b     |   %b       |         %b       |       %b      |     %b       |       %b          |      %b      |      %b     |  %b     |     %b     |   %b | %0d ",
            currentPC, DataOut, I31_0, ID_ALU_op, Data_Mem_Opcode, ID_shift_imm, ID_load_instr, ID_RF_enable, ID_B_instr, ForwardA, ForwardB,
            EX_ALU_op, EX_shift_imm, EX_load_instr, EX_RF_enable, cond_output, EX_Data_Mem_Opcode,
            MEM_load_instr, MEM_RF_enable, MEM_Data_Mem_Opcode,
            WB_load_instr, WB_RF_enable, MEM_ALU_Res, cond_output,
                   Clk, $time);*/
          
/*Official display 
         $display("    PC    |             I                  |           MEM_ALU_Res                   |                    R1                   |                     R2                  |                        R3               |                   R15             |Clk| Time "); 
          $monitor("%d|%b|  %b       |  %b       |  %b       |  %b       |  %b | %d | %0d",
            currentPC, DataOut, MEM_ALU_Res,Register_File.R1.Q,Register_File.R2.Q,Register_File.R3.Q, Register_File.R15.Q,
            Clk, $time);
*/ 
          //AMYS DISPLAYYYYY
          $display("    PC    |             I                  |            I31_0               |ID_ALU_op|Data_Mem_Opcode|ID_shift_imm|ID_load_instr|ID_RF_enable|ID_B_instr|ForwardA|ForwardB|EX_ALU_op|EX_shift_imm|EX_load_instr|EX_RF_enable|cond_output|EX_Data_Mem_Opcode|MEM_load_instr|MEM_RF_enable|MEM_Data_Mem_Opcode|WB_load_instr|WB_RF_enable|          MEM_ALU_Res                  |cond_output|  Clk| Time  | R14"); 
          $monitor("%d|%b|%b|   %b  |     %b        |      %b     |      %b      |      %b     |     %b    |  %b    |  %b    |  %b   |    %b       |      %b      |      %b     |   %b       |         %b       |       %b      |     %b       |       %b          |      %b      |      %b     |  %b     |     %b     |   %b | %0d |",
            currentPC, DataOut, I31_0, ID_ALU_op, Data_Mem_Opcode, ID_shift_imm, ID_load_instr, ID_RF_enable, ID_B_instr, ForwardA, ForwardB,
            EX_ALU_op, EX_shift_imm, EX_load_instr, EX_RF_enable, cond_output, EX_Data_Mem_Opcode,
            MEM_load_instr, MEM_RF_enable, MEM_Data_Mem_Opcode,
            WB_load_instr, WB_RF_enable, MEM_ALU_Res, cond_output,
                   Clk, $time, );
           
          end 
  
endmodule
