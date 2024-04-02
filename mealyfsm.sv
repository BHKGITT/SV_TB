//////////////////////////////////////////////PACKAGE////////////////////////////////////////////
package fsm_pkg;
	int no_of_txn;
endpackage
//////////////////////////////////////////////DUV////////////////////////////////////////////
//FSM Mealy Overlapping for seq 1011
import fsm_pkg::*;

module mfsm(input clock,reset,seq_in,output reg out);
//States declaration by Parameter
parameter A=2'b00,
	  B=2'b01,
	  C=2'b10,
	  D=2'b11;
//Declaring Present_state & Next_state var
reg[1:0] PS,NS;
//Present State LOgic
always@(posedge clock)
	begin
		if(reset)
			PS<=A;
		else
			PS<=NS;
	end
//Next State Logic
always@(*)
	begin
		NS=A;
		case(PS)
			A:if(seq_in)
				NS=B;
			  else
				NS=A;
			B:if(seq_in)
				NS=B;
			  else
				NS=C;
			C:if(seq_in)
				NS=D;
			  else
				NS=A;
			D:if(seq_in)
				NS=B;
			  else
				NS=C;			
		endcase
	end
//Output Logic
always@(posedge clock)
	begin
		out<=1'b0;
		case(PS)
		A,B,C:out <=1'b0;
		   D:if(seq_in)
			out<=1'b1;
		     else
			out<=1'b0;
		endcase
	end
endmodule

//////////////////////////////////////////INTERFACE /////////////////////////////////////////////////
interface fsm(input bit clock);
//Signals
	logic reset;
	logic seq_in;
	logic out;

clocking wr_drv_cb@(posedge clock);
default input #1 output#0;
	output reset;
	output seq_in;
endclocking

clocking wr_mon_cb@(posedge clock);
default input #1 output#0;
	input reset;
	input seq_in;
endclocking

clocking rd_mon_cb@(posedge clock);
default input #1 output#0;
	input out;
endclocking

modport WDRV_MP (clocking wr_drv_cb);
modport WMON_MP (clocking wr_mon_cb);
modport RMON_MP (clocking rd_mon_cb);

endinterface

/////////////////////////////////////// TXN ////////////////////////////////////////////////////////

class trans;
	rand bit reset;
	rand bit seq_in;
	     bit out;
	
	constraint c1{reset dist{1:=5,0:=45};}
	constraint c2{seq_in dist{1:=15,0:=15};}

	function void display(input string message);
		$display("------------------------------------------------------------\n");
		$display("%s",message);
		$display("reset = %b",reset);
		$display("seq_in = %b",seq_in);
		$display("out = %b",out);
		$display("------------------------------------------------------------\n");	
	endfunction
endclass

/////////////////////////////////////GENERATOR//////////////////////////////////////////

class generator;
	trans gen_txn,gen_txn1;

	mailbox#(trans)  gen2dr; 
	
	function new(mailbox#(trans) gen2dr);
		this.gen2dr = gen2dr;
		this.gen_txn=new;
	endfunction
	
	virtual task start();
		fork
			for(int i=0; i<no_of_txn;i++)
				begin
					assert(gen_txn.randomize());
					gen_txn1=new gen_txn;	//Shallow copy
					gen_txn1.display("GENERATED TXNS");
					gen2dr.put(gen_txn1);					
				end
		join_none
	endtask
endclass
/////////////////////////////////////WR_DRIVER//////////////////////////////////////////
class write_driver;
	virtual fsm.WDRV_MP wdrv_if;
	trans 	data2duv; //For Getting txn's from mailbox
//	mailbox#(trans) wdrv2duv;
	mailbox#(trans) gen2dr; 
	function new(virtual fsm.WDRV_MP wdrv_if,mailbox#(trans) gen2dr);
		this.wdrv_if = wdrv_if;
		this.gen2dr = gen2dr;
	endfunction

	virtual task start();
		fork
			forever
				begin
					gen2dr.get(data2duv);
				//	data2duv.display("WRITE DRIVER DATA");
					drive();	//user defined task
				end
		join_none
	endtask
//User defined task
	virtual task drive();
		@(wdrv_if.wr_drv_cb);
		wdrv_if.wr_drv_cb.reset <= data2duv.reset; 
		wdrv_if.wr_drv_cb.seq_in <= data2duv.seq_in;
	endtask
endclass
/////////////////////////////////////WR_MONITOR//////////////////////////////////////////
class write_monitor;
	virtual fsm.WMON_MP wmon_if;
	trans 	duv2wrmon; //For Getting data from interface
	trans	wrmon2rm;
	mailbox#(trans) wr2rm;
	function new(virtual fsm.WMON_MP wmon_if,mailbox#(trans) wr2rm);
		this.wmon_if = wmon_if;
		this.wr2rm = wr2rm;
		this.duv2wrmon=new;
	endfunction

	virtual task start();
		fork
			forever
				begin
					monitor();	//user defined task
					wrmon2rm = new duv2wrmon;
				//	wrmon2rm.display("WRITE MONITOR DATA");
					wr2rm.put(wrmon2rm);
				end
		join_none
	endtask
//User defined task
	virtual task monitor();
		@(wmon_if.wr_mon_cb);
		@(wmon_if.wr_mon_cb);
		duv2wrmon.reset = wmon_if.wr_mon_cb.reset;  
		duv2wrmon.seq_in = wmon_if.wr_mon_cb.seq_in;
	endtask
endclass
/////////////////////////////////////RD_MONITOR//////////////////////////////////////////
class read_monitor;
	virtual fsm.RMON_MP rmon_if;
	trans 	duv2rdmon; //For Getting data from interface
	trans	rd2sb;
//	mailbox#(trans) rddata2rm;
	mailbox#(trans) rdmon2sb;
	function new(virtual fsm.RMON_MP rmon_if,mailbox#(trans) rdmon2sb);
		this.rmon_if = rmon_if;
	//	this.rddata2rm = rddata2rm;
		this.rdmon2sb = rdmon2sb;
		this.duv2rdmon=new;
	endfunction

	virtual task start();
		fork
			forever
				begin
					monitor();	//user defined task
					duv2rdmon.display("READ MONITOR DATA");
					rd2sb = new duv2rdmon; //Shallow copy
				//	rddata2rm.put(rd2sb);
					rdmon2sb.put(rd2sb);				
				end
		join_none
	endtask
//User defined task
	virtual task monitor();
		@(rmon_if.rd_mon_cb);
		@(rmon_if.rd_mon_cb);
		duv2rdmon.out = rmon_if.rd_mon_cb.out;  
	endtask
endclass
/////////////////////////////////////REF_MODEL//////////////////////////////////////////
class ref_model;
	trans 	ref1,ref2; 
//	logic[1:0] present;
	typedef enum bit[1:0] {A,B,C,D} statetype;
	statetype present;
	static logic ref_out;
	mailbox#(trans) wr2rm;
//	mailbox#(trans) rd2rm;
	mailbox#(trans) ref2sb;
	function new(mailbox#(trans) wr2rm,mailbox#(trans) ref2sb);
		this.wr2rm = wr2rm;
	//	this.rd2rm = rd2rm;
		this.ref2sb = ref2sb;
	endfunction

	virtual task start();
		fork
			forever
				begin
					wr2rm.get(ref1);
					golden_logic(ref1);  //ref model logic
					ref1.out <= ref_out; //Assign expected output to ref1 handle
					ref1.display("REFERENCE MODEL DATA");
					ref2sb.put(ref1);				
				end
		join_none
	endtask

	virtual task golden_logic(trans ref1);
	/*	if(ref2.reset)
			present <= state.A;
		else 
			present <= next;
		else if
			begin
				case(present)
				state.A:
				state.B:
				state.C:
				state.D:	
				endcase
			end  */
		if(ref1.reset)
			ref_out<=1'b0;
		else
			begin
				case(present)
					A,B,C:ref_out <= 1'b0;
		   			D:if(ref1.seq_in)
						ref_out <= 1'b1;
		   		 	  else
						ref_out <= 1'b0;
				endcase
			end

	endtask
endclass
/////////////////////////////////////SCOREBOARD//////////////////////////////////////////
class scoreboard;
	trans 	sb1,sb2;
	event DONE;
	static int ref_data,mon_data,verified; 
	mailbox#(trans) rdmon2sb;
	mailbox#(trans) ref2sb;
	function new(mailbox#(trans) rdmon2sb,mailbox#(trans) ref2sb);
		this.rdmon2sb = rdmon2sb;
		this.ref2sb = ref2sb;
	endfunction

	virtual task start();
		fork
			forever
				begin
					ref2sb.get(sb1);
					ref_data++;					
					rdmon2sb.get(sb2);	
					mon_data++;
					compare(sb2);	 //calling compare user defined task		
				end
		join_none
	endtask
//User defined compare task
	virtual task compare(trans sb3);
		begin
			if(sb1.out == sb3.out)
				$display("MATCHED DATA");
			else
				$display("MATCH FAILED");
		end
			verified++;
			if(verified >= no_of_txn+2)
				begin
					->>DONE;
				end
	endtask
//Report method
	virtual task report();
		$display("-------------------REPORT---------------------");
		$display("Gnerated data: %0d",mon_data);
		$display("Verified data: %0d",verified);
		$display("----------------------------------------------");	
	endtask
endclass
/////////////////////////////////////ENVIRONMENT//////////////////////////////////////////
class environment;
//Declare all Virtual interfaces
	virtual fsm.WDRV_MP wdrv_if; 
	virtual fsm.WMON_MP wmon_if; 
	virtual fsm.RMON_MP rmon_if;

//Declare all mailbox handle
	mailbox#(trans) gen2dr=new;  		//GEN
	mailbox#(trans) gen2dr=new;		//DRV
	mailbox#(trans) wr2rm=new;		//WMON
	mailbox#(trans) rdmon2sb=new;		//RMON
	mailbox#(trans) wr2rm=new;		//REF
	mailbox#(trans) ref2sb=new;		//REF
	mailbox#(trans) rdmon2sb=new;		//SB
	mailbox#(trans) ref2sb=new;		//SB

//Declare all transactor handles
	generator		gen_h;
	write_driver		wdrv_h;
	write_monitor		wmon_h;
	read_monitor		rmon_h;
	ref_model		ref_h;
	scoreboard		sb_h;

	function new(virtual fsm.WDRV_MP wdrv_if,virtual fsm.WMON_MP wmon_if,virtual fsm.RMON_MP rmon_if);
		this.wdrv_if = wdrv_if;
		this.wmon_if = wmon_if;
		this.rmon_if = rmon_if;
	endfunction
//build() task to create instances for transactors and pass args
	virtual task build();
		begin
			gen_h   = new(gen2dr);
			wdrv_h  = new(wdrv_if,gen2dr);
			wmon_h  = new(wmon_if,wr2rm);
			rmon_h  = new(rmon_if,rdmon2sb);
			ref_h	= new(wr2rm,ref2sb);
			sb_h	= new(rdmon2sb,ref2sb);		
		end
	endtask
//start() task to run every transactor start() methods
	virtual task start();
		begin
			gen_h.start();
			wdrv_h.start();
			wmon_h.start();
			rmon_h.start();
			ref_h.start();
			sb_h.start();		
		end
	endtask

	virtual task stop();
		wait(sb_h.DONE.triggered);
	endtask
//run() method
	virtual task run();
		start();
		stop();
		sb_h.report();		
	endtask
endclass
/////////////////////////////////////TEST//////////////////////////////////////////
class test;
//Declare all Virtual interfaces
	virtual fsm.WDRV_MP wdrv_if; 
	virtual fsm.WMON_MP wmon_if; 
	virtual fsm.RMON_MP rmon_if;
	
	environment	env_h;

	function new(virtual fsm.WDRV_MP wdrv_if,virtual fsm.WMON_MP wmon_if,virtual fsm.RMON_MP rmon_if);
		this.wdrv_if = wdrv_if;
		this.wmon_if = wmon_if;
		this.rmon_if = rmon_if;
		this.env_h = new(wdrv_if,wmon_if,rmon_if);
	endfunction
//build() task to create instances for transactors and pass args
	virtual task build();
		env_h.build();
	endtask
//run() method
	virtual task run();
		env_h.run();		
	endtask
endclass
/////////////////////////////////////TOP//////////////////////////////////////////
module top();
//import fsm_pkg::*;
//Declare clock
	reg clock;
//Instantiate interface
	fsm IF(clock);
//Declare test class handle
	test test_h;
//Instantiate DUV
	mfsm  DUV(.clock(clock),.reset(IF.reset),.seq_in(IF.seq_in),.out(IF.out));

//Clock generate
initial
	begin
		clock = 1'b0;
		forever#5 clock = ~clock;
	end
initial
	begin
		test_h=new(IF,IF,IF);
		no_of_txn = 30;
		test_h.build();
		test_h.run();
		$finish;
	end
endmodule

