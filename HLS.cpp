HARDWARE:-OUTPUTS
SOFTWARE:-CALL BY REFERENCE ARGUMNETS &FUCNTION RETURN AND NOT USE CALL BY VALUE ARGUMENT 
HARDWARE:-INPUTS
SOFTWARE:-CALL BY VALUE ARGUMENTS
HARDWARE:-INOUTS
SOFTWARE:-CALL BY REFERENCE ARGUMENTS
HARDWARTE:-DESIGN 
SOFTWARE:-FUNCTION BODY 
A SOFTWARE FUNCTION DESCRIBE HARDWARE MODULE 
THE FUNCTION ARGUMENT MODEL THE HARDWARE PORTS 
A FUNCTION BODY IMPLEMENTS HARDWARE BEHAVIOUR 


TO USE THE HARDWARE PORTS 

void ledonoff(unsigned char*o)
{*o=0b11110000
}
port and function interfaces 
void ledonoff(unsigned char*o)
{
#pragmaHLS INTERFACE AP_NONE  PORT=o                                 ##AS WE DON'T HAVE ANY SPECIFIC INTERRFACE IN THIS EXAMPLE AP_NONE is used for this purpose 
#pragmaHLS INTERFACEAP_CTRL_NONE PORT=return                                                                   ##there is also an interface assigned to this whole function 

*o=0b11110000
}


In vivado HLS we 
1.Create a project 
2.add a source file 
3.perform high level synthesis
4.generate IP

TOP FUNCTION :IS THE NAME OF THE HARDWARE MODULE ENTRY FUNCTION .EACH HARDWARE MOPDULE HAVE ON TOP FUNCITON


VIVADO HLS GUI HAS 4 PARTS 

1st has EXPLORER it has includes which is the included header file 
2. source 
3.test bench 



void basic_output(unsigned char*o){
	*o=0b11110000;
}

#Now we should add pragma 
1.adding manually to the source code 
2. use directive 

void FIRST(unsigned char *o){
#pragma HLS INTERFACE ap_none port=o
#pragma HLS INTERFACE ap_ctrl_none port=return
	*o=0b11110000;
}
after it use synthesis and RTL .
we will get the ip then open vivado go to create design and after right click import the ip then click on the plus sign and add the IP .L
there are two pragma 
1.top level function pragma which add the control signal to the synthesized hardware.
2.Port related pragma  
since currently our design is simple thus these two group of pragma doesnot define the right interface.


DESIGN IN VIVADO 


LVCMOS33 
3.3 V LOW VOLTAGE CMOS SINGLE ENDED I/O STANDARD 
TO CONNECT THE IP PORTS INTO THE FPGA PINS ,WE SHOULD DEFINE SOME CONSTRAINTS 
THE MAIN CONSTRAINTS DETERMINE THE PIN LOCATION AND PIN ELECTRICAL STANDARD 



SIMPLE INPUT 
##BASIC INPUT OUTPUT 


#define data_type  unsigned char 

void  basic_input_output(
data_type input
data_type *output){*output=input;}

WHen the top level function is synthesized, the arguments (parameter)to the function are synthesized into RTL ports.This process is called interface synthesis.


port  interface can have various modes

a top funccitoppn in HLS cannot return a pointer value 

#define data_type  unsigned char

void PORT(
data_type input,
data_type *output)
{
#pragma HLS INTERFACE ap_none port=output
#pragma HLS INTERFACE ap_none port=input
#pragma HLS INTERFACE ap_ctrl_none port=return ##ports are assinged as simple wire .

	*output=input;
}

gate propagation delay 
we assume here tp as the gate propagation delay
it takes time for the output to reflect input state changes 
one of opur main goal in hls is to reduce this delay and decreasing this design response time .
serially connected hardware increases the corresponding propagation delay .
As we are using wires to carry logic values we use ap_none or ap_ctrl_none as interface .
Now three parts are usefull for us now i.e timing utilisation and interface .
In timing part is the estimation of the design propagation delay 

HLS REPORT OF COMBINATIONAL CIIRCUIT SYNTHESIZED BY VIVADO HLS 
1.SHOULD NOT UTILISE ANY MEMORY ELEMENT 
2.SHOULD NOT HAVE ANY CLOCK PORT IN THE INTERFACE PART 


##now to analyse use the analysis which tells about the synthesis .
Control states are the internal state that high level synthesis uses to schedule opeeration
combinational circuit perform all its logical operation in a single state denoted by 0.


RULES FOR USING FUNCTIONS IN VIVADO HLS 

1.MUST CONTAIN THE ENTIRE FUNCTIONALITY .
2.THEY CAN BE ORGANISED INTO THE LIBRARIES.
SYSTEM CALLS TO THE OPERATING SYSTEM ARE NOT ALLOWED .
LIKE SLEEEP(),GETC(),TIME()
THESE CAN BE USED IN HTE TEST BENCH FUNCTION BUT NOT IN THE DESGIN FUNCTION
VIVADO HLS IGNORES COMMONLY USED SYSTEM CALLS SUCH AS COUT AND PRINTF
TOP FUNCTION CANNOT BE A STATIC FUNCTION
 
 
 INTEFACE SYNTHESIS 
 ARGUMENT IN THE TOP LEVEL FUNCTION ARE DEFINED AS THE PORTS 
 SUB FUNCTIONS CAN BE INLINED TO MERGE THERE LOGIC WIHT THE LOGIC OF THE SURROUNDING FUNCTION .
 INLING CAN ALSO INCRESE THE RUN TIME.
 
 
##now to see how can we model a structure of a combinational code inside a function to better understand the synthesis process behaviour  
 bool not_gate(bool a ){
#pragma HLS INLINE off


	return !a;

}
bool and_gate(bool a,bool b){
#pragma HLS INLINE off
	return(a & b);
}

void automatic_inline(bool a ,bool b,bool *c){
#pragma HLS INTERFACE ap_none port=c
#pragma HLS INTERFACE ap_none port=b
#pragma HLS INTERFACE ap_none port=a
#pragma HLS INTERFACE ap_ctrl_none port=return
	*c= and_gate(not_gate(a),  b);
}


DATA FLOW REPRESENT MOVEMENT OF A DESIGN FROM INPUT TO A OUTPUT 
THE GRAPH IS A DIRECTED GRAPH IN WHICH OPERATOR SUCH AS LOGICAL ARITHMETIC AND ASSIGNMENT OPERATOR ARE REPRESENTED BY THE GRAPH NODES AND INFORMATION FLOW IS REPRESNTED BY THE ARGS 
 ACYCLIC DATA FLOW GRAPH IS A DATA FLOW GRAP[H WITHOUT A CYCLE PATH  



C TEST BENCH 

COMBINATION OF DESIGN HAS AN INPUTS AND OUTPUTS .THE ROLE OF SIMULATION IS TO APPLY ALL POSSIBLE INPUTS AND TO CHECK THE OUTPUTS COMPARING WITH THE GOLDEN MODEL PRPOVIDED BY THE DESIGN SPECIFICATION.
SIMULKATION IS A VERIFICATION TECHNIQUE PROVIDED BY THE HLS TOOL 

TEST BENCH HAS  4 PARTS

1.GENERATE INPUTS
2.APPLY INPUTS TO THE DESIGN 
3.APPLY INPUTS TO THE GOLDEN MODELS
4.COMPARE THE OUTPUTS OF THE DESIGN AND THE GOLDEN MODEL.


NATIVE C++ DATA TYPE 
BOOL ,CHAR ,INT 
MODIFIER SIGNED UNSIGNED LONG
FLOAT ,DOUBLE DATA TYPE WHICH IS ALSO CALLED SINGLE PRECISION FLOAT IS OF 32 BIT AND DOUBLE DATA TYPE IS OF 64 BIT .
vivdo HLS support float and double data type 
for combionational we donot use memory cell so all are mapped in the wire 

clock period constrained should be higher than the circuit propagatrion delay


BIT PRECISION DECLARATION

VIVADO PROVIDE AN ARBITRARY PRECISION DATA TYPE LIBRARY 
THIS LIBRARY SUPPORTS BOTH ARBITRARY PRECISION DATA TYPES FOR BOTH SIMULATION ADN SYNTHESIS
IT COMES IN TWO FLAVOUR 
1. FOR C 
2 .FOR C++

INTEGER PRECISION DATA TYPE
IN ORDER TO USE THIS LIBRARY WE HAVE TO USE HEADER FFILE 

#include<ap_int.h>
example 
#include "ap_int.h"
ap_int<5> a;# 5 bit signed integer value 
ap_uint<11> b;8 bit unsigned integer value 

ap int and ap uint template provides all the arithmetic,bitwise ,logical and relational operator 

this LIBRARY SUPPORTS ARBITRARY BIT WIDTH 1 TO 1024 BITS ##we can overwrite this default by 

#define AP_INT_MAX_W  8192
#include "ap_int.h"
ap_int<8192>wide_a;

##WHAT ARE HTE RULES OF ASSIGNING VALUES TO THESE VARIABLE 

VARIABLE ASSIGNMEBT IS POSSIBLE BECAUSE OF THE CLASS CONSTRUCTOR AND OVERLOADED ASSIGNMENT OPERATOR 

ap_int<17>a=0x17e;
ap_uint<57>b=0xef42ed12cf72abULL;#it can represent number upto 64 bits.
To allow assignment of values wider than 64 bits constructors are provided

CONSTRUCTOR GETS TWO ASSIGNMENT A STRING REPRESENTING A VALUES AND A RADIX.
ap_[u]int<w> a=ap_[u]int<w>(string,radix);
ap_[u]int<w> a (string,radix);

ap_int<41> a1=04017;
ap_int<41> a1=ap_int<41>("4017",8);
ap_int<763> a1=ap_int<763>("4017401740174017",16);
values can be printed in the test bench using "cout" 
std::cout<<"b="<<std::oct<<b<<std::endl;


ASSIGNMENT USING BIT PRECISION 

=,+=,-=,*=,/=,%=,<<=,>>=,&=^=,|=.

IF BIT WIDTH OF TWO VARIABLE ARE NOT THE SAME 

ap_int<10>a;
ap_uint<17>b;
ap_int<8>c;

3 Technique handle this situation 
1.SIGN EXTENSION
2.ZERO EXTENSION
3.TRUNCATION

ap_int<7> a=0x5a;#1011010
ap_int<10>b;#1111011010
ap_uint<7> a=0x5a;
ap_int<10>f ;#0001011010
f=a;

TRUNCATION 

IT CAN LEAD TO UNEXPECTED BEHAVIOUR.


##bit precision in print 

the memeber function called to_string converts the valueof an arbitrary precision integer data types into the corresponding C++ string .



##BIT LEVEL OPERATION AVAILABLE IN VIVADO HLS 
1.length () memeber fucntion return the widht of an arbitrary precision data types 
ap_int<41>a=0x23462af;
int len=a.length();
len=41;
#THIS FUCNITON IS SYNTHESIZABLE AND CAN BE USED IN DESIGN AND TEST BENCH FILE 

##CONCATENATION :the concat member function combines two variable \
ap_int<5> b=0x3;
ap_int<7> c=0x13;
ap_int<12> r1=b.concat(c);
ap_int<12> r1=(b,c);
##OVERLOADED OPERATOR 

LSB POSIITON IS 0 AND THE MSB IS 6 FOR A 7 BIT
ap_int<7> c=0x2f;
int i=4;
ap_uint<1> one_bit=n[i];
n[i]=1;

using the range memeber function

ap_uint<4>r;
ap_uint<9>p=0xef;
r=p.range(3,0);


BITWISE LOGICAL OPERATOR 
THEY ARE TRATED AS UNSIGNED ONLY IF BOTH OPERAND ARE OF THE SIGNED TYPE 
BITWISE OR |
BITWISE AND &
BITWISE XOR ^
BITWISE INVERSE ~

ap_int<7> a=0x2f;
ap_int<7> b=0x31;


REDUCE LOGICAL FUNCTION 

IT APLIES AN OPERATOR TO ALL BITS 
AND reduce....... and_reduce()
OR reduce....... or_reduce()
NAND reduce....... nand_reduce()
XOR reduce....... xor_reduce()
NOR reduce....... nor_reduce()
XNOR reduce....... xnor_reduce()

for example 
ap_int<7>a=0x2f;
reduced xor =1


#BIT REVERSE 

reverse()
ap_int<7>a=0X2f;;
a.reverse();


ap_int<7>a=0X2f;;
##TEST /SET/CLEAR.
test(i)  bol t=a.test(5)
returns TRUE if yes and FALSE if no.
set(i,v)  a.set(5,0);
bool t=a.test(5);
sets the denoted bit 5 to 0

set(i) sets the denoted bit specified by i to 1

clear(i) sets the denoted bit by 0;


##SHIFT OPERATOR 

OVERLOADED SHIFT OPERATOR AVAILABLE FOR HLS ARBITRARY PRECISION DATA TYPES 

THERE ARE TWO BITWISE SHIFT OPERATOR RIGHT SHIFT (>>) AND LEFT SHIFT <<

ap_int<7>a=0X2f;

ap_int<7>b=a>>3;


ap_int<7>a=0X2f;

ap_int<7>b=a<<3;
a<<-3 means a>>3
b.rrotate(3);
b.lrotate(3);


To drive a set of 7 segment two group of signal are required 
1 DATA signal 
2.CONTROL signal 

Common anode 0 = ON and 1= OFF
Common cathode 0=OFF and 1=ON

TAKEAWAY 

design steps 

1. get the digits 
2.encode the digit 
3.drive the 7 segment with the code 

BCD code types 

1.PACKED:- IT NEEDS ONE BYTES 
2.UNPACKED :-IT NEEDS TWO BYTES 


DESIGNING A CIRCUIT TO CONVERT ITS BINARY TO BCD FORMAT IS OUR NEXT TOPIC


Combinational loop

for(int i=0;i<3;i++)
task1;
task2;
}


Scenario 1
each task is dependent on the previous task sequential is expected 
Scenario2
all task1 can be executed in parallel 
all tassk2 can be executed in parallel

Scenario 3
all task are independent as they work on different data 

FOR EX:
an N bit binary adder is a combinational hardware circuit with a repetitive computational pattern

it uses N full adder in order to add 2 N bit binary number 


N bit is a one counter which gives binary representation of number of ones in a binary representation of a given number .


for loop condition to describe repetitive combinational pattern in HLS 


FOR UNROLLING CONCEPT 

ONLY LOOPS WITH STATIC BOUND WHICHARE FIXED AND KNOWN AT COMPILE TIME CAN BE UNRROLLED BY THE HLS TOOL

#define N 100
for(int i=0;i<N;i++)
{
#pragma HLS UNROLL
task();
}
To unroll a loop completely
to loop bounds must be known at the compile time .
there should be enough resources on FPGA e.g enough LUT

Parity Bit Definiton
Parity Bit is a technique to detect single bit error in a binary data 
Parity is a simple mechaniosm to detect single bit error in a binary data 
a parity generator receive the data and generate the parity 
Laater the parity bit checker check the parity of the data and parioty and tell if there are any error.

There are two types of Parity odd parity:i.e the total number of ones in abinary value is odd and even parity 

Parity for W bit data

p =d[0] xor d[1] xor d[2] xor d[3] xor d[4] xor d[W-1]
pe=p
po=not(p)
Totl propagation delay is =(W-1)*delta 

How to reduce circuit propagation delay 

The chain of XOR gates can implement the parity bit generator 

How CAN WE REDUCE PROPAGATION DELAY 
TO MAKE THE CODSSE IN THEOFMR OF A BALANCED TREE
THEREFORE THIS BALANCED TREE IMPLEMENT THE FORM OF THE GENREATOR


DESCRIBE PARITY BIT GENERATOR IN HLS

an unrolled for-loop representing the parity bit genrator can be synthesized combinational circuit.

HLS can synthesize basic arithmetic operator into the combioantional circuit without any extra work for designer 

Basic arithmetic operator 
+,-/,*,%


there are two types of arithmetic operator in vivado HLS combiantional and pipelined

the pipelined implementation  uses a set of memeory cells called flip flop


##if clock period is greater than  desgin propagation delay then iti ssynthesized into the combinational circuit 
##if clock peirod is less than design  propagation delasy then it is synthesized into the sequebntial ciruuit 


HLS tool can automatically synthesize arithmetic operations on the native C data types
Combinational circuits can implement an arthimetic expression
the arthimetic operation can be implemented by LUT or DSP.



OPERATOR OVERLOADING 
WIRTING
There is clock constraints to each HLS design.

DESIGN YIMING CONSTRAINTS
IF THE TOTAL PROPAGATION DELAY IS LESS THAN A CLOCK PERIOD AND THE DESIGN DOESNOT USE ANY MEMORY CELL THE HLS TOOLDROP THE CLOCK SIGNAL DURING SYNTHESIS AND GENRATE A FULLY COMBIANTIONAL CIRCUIT
IF THE CLOCK PERIOD IS LESS THAN THE PROPAGATION DELAY THE HIGH LEVEL SYNTHESIS TOOL AUTOMATICALLY SCHEDUELS THE 

DSP RESOURCES 

TRADITIONALLY FPGA USES LOGIC GATES TO DESCRIBE ARTHIMETIC EXPRESSISON THIS SOLUTION IS ADEQUATE WITH A FEW ARITHMETIC OPERATION HOWEVER APPLICATION WITH COMPLEX CMPUTAITON WILL REQUIRE MORE
EFFICIENT IMPLEMENTATION TECHNIQUE TO COPE WITH THIS DEMAND ADVANCED FPGA REQUIRE DSP BLOCK CAPABLE OFG PERFORMING HIGH PERFORMANCE ARITHMETIC OPERATION .
THUS THERE ARE TWO DIFFERENT RESOURCES LOOK UP TABLE AND DSP RESOURCES .


DSP IS THE MOST CPOMPLEX COMPUTATIONAL BLOCK AVAILABLE IN FPGA XILINK.
A TYPICAL DSP BLOCK PASS IT THROUGH A TYPICAL ADDITION AND RECONFIGURABLE MODULE.
AL,MOST ALL THE DATA PATH INSIDE THE DSP ARE RECONFIGURABLE 

HLS TOOL CONFIGURE ALL THE TASK AUTOMATICALLY 

DSP STRUCTURE 
1. MULTIPLIER ACCEPTS A 25 BIT SAND 18 BIT 2'S COMPLEMENT INPUT  AND GENRATE A 43 BIT 2'S COMPLEMENT OUTPUT.
INPUTS ARE VIS PRE ADDER MODULEA DN GIVE RESULT TO POST ADDER OUTPUT








####HLS DESIGN WITH ITS FOCUS ON FPGA CIRCUIT USING SEQUENTIAL CIRCUIT


HW/SW CONCEPT

STEPS 
1.DESIGN
2.SIMULATION
3.ANALYSIS
4.SYNTHESIS
5.IMPLEMENTATION
6.BITSTREAM


D Flip Flop 

Memory Cell

Circuit which store Data temporially or permanently these types of circuits are called sequential circuit 


Flip Flop has two input signal which are DATA AND CLOCK and the ouput signal isthe flip flop state.
The data cannot be saved in the memory Until the clock gives the permission.this permission happens at the edge of the clock .

How Does a memory cell use a typical FPGA in a sequential circuit?
sequential circuit has the combinational circuit that perform the logic of the task that stores the state execution sequance and transfer data between these sequences
As memories commonly use D flip flop as tthere building blocks a clock signal should determine the time boundaries between two sequences of execution and triggers saving the state and data generated by the combinational circuit.


Combinational circuit should use the input data as well as the data in the memory to generate output and the new memory content.


Here we consider Combiantional circuitdelaay to be less than the clock period.


##SETUP AND HOLD TIME:-
THERE ARE TWO TIMING BEFORE AND AFTER THE RISING EDGE BEFORE THE DATA IS STABLE AND MUST NOT CHANGE .
THE TIMING WINDOW BEFORE THE EDGE IS CALLED THE SETUP TIME AND THE ONE AFER THE EDGE IS CALLED HOLD TIME .
DURING THIS TIME THE D FLIP FLOP SAMPLE THE DATA AND SAVES INTO ITS STATE.
DURATION DEPEND ON THE UNDERLYING TECHNOLOGY IMPLEMENTING THE FLIP FLOPS  THUS THEY ARE FIXED AND CANNOT BE CHANGED OR CONTROLED.
THUS THE COMBINATIONAL CIRCUIT SHOULD PROVIDE THE DATA BEFORE THIS WINDOW AND KEEP THE DATA UNCHANGED DURING THE FLIP FLOP SAMPLING PERIOD .
THERE IS A MARGIN BETWEEN combiantional circuit propagation delay and setup time .designer con sider some margin in the clock timing.
the clock period conssit of two main part the combination logic propagation delay and uncertainity .Clock uncertainity is used for High level synthesis.
Uncertainity value provides a controllable margin. to account fopr any increses inner delays due to RTL logic synthesis.


the default value of clock uncertainity in vitis HLS is 27% of the clock period.



##STATE
 it encpasulate past input and output upto a specific time before going to next state .Thus transition can describe behaviour of sequential circuit
 
 
 ###IN general case a sequential circuit with n memeory cells can have p states where p= 2^n
 reset signal has the resposibilitites to put circuit into a known state.


ACTIVE HIGH :- THE RESET HIGH CLEARS THE TARGET .
ACTIVE LOW:- THE RESET LOW CLEARS THE TARGET .


if it reset changes without depending on the clock asynchronous reset
if it wait for following edge to change it is the synchronous rest

SHIFT REGISTER :- ON EACH CLOCK EDGE THE DATA IS SHIFTED ONE BIT TO THE RIGHT .
##PARALLEL LOAD SHIFT REGISTER 

if combinational propagation delay is less than the time period 
In each clock cycle the combinational circuit reads the current circuit from the memory cells ,generates the output and the next state ,save the next state.
Sequential cicuit design idea :-

void func(arguments){
define registers;#We use static variables defintion in C/C++ to declare registers(to keep the circuit states) in HLS 
read the registers values
 perform tasks
 modify register values}
 
 
 ONE CLOCK CYCLE DESIGN
 
 PARALLEL TO SERIAL CONVERSION 
 
 we put a parrralel input in the serial and here we have the d , start ,end and begin.
 
SERIAL TO PARALLEL CONVERTER:-
VARIABLES ARE 
D,START,END ,A.





A C based test bench should be provided fir the design funcitonality and timing validation
A C based test bench can genrate different input waveform as the test vectors for the design under test.

Simulation is one of the verification technique to check the validity of the design is done in two ways 
1 Pre synthesis validation.
2 post synthesis validation.

role of testt bench is to genrate the inputs and get the outputs  and compare it to tte golden model to check if the output is right.

C++ main function should be used as the testbench top function.

int main(){
Define Variables
genrate input test vector
call design top function
get the result from golden model 
compare results
return status.
}
Test bench must not require interactive user inputs during execution.


TO CREATE AN INPUT WAVEFORM TO TEST ITS FUNCITONALITY AND TIME 

STATE MACHINE 

FSM is used to model a wide range of sequential cirucit .therefore describing an FSM in HLS is essential 

FSM is amodelling technique representing a sequence of event in an algorithm .
If an application consit of list of well defined task which determine all the possibnle states transition and design outputs in terms of all the input combination then FSM is used 


A finite state machine can model an application with the list of well defined tasks 
a state transition diagram can represent an FSM which later will turned tro the equivalent HLS code .
FSM CONCEPTS 
STATES 
TRASITION


FSM ELEMENTS 
1.REGISTERS:TO KEEOPP THE STATES 
2.CONDIITONS :DEFIENS ALL THE TRANSITION
3.CONDIIITONAL STATEMNETS:SWTICH CASE 

##THERE IS NO CONDITION FOR THE TRANSITION BETWEEN THE TWO STATE WITH THE RISING EDGE OF THE CLOCK YOU WILL ALWAYS GO FROM ONE STATE TO THE OTHER 


|##HLS CODE TEMPLATE TO RREPRESNT AN FSM 

TEMPLATE FOR THE FSM 
enymerate types to define the lists of the states
void functiion_name(argument){

1.states by defining the static varibles 
2.next sate variables definition#idea is that we are not going to change the regicsters values sseveral time sso wwe willl only change the next states throughout the funciton 
and assign the value of the registers at the end
3. then we assign a variaable corressponding to each output argument
4 then we have the switch case that define all the FSM transtion .
5. at the end of the function we modify the state register an dassign the output to the output argument }

}
typedef enum{S0,S2,S23,S234,S2346} state_type;
void sequence_finder(ap_uint<4> x,bool &door_open){
#pragma HLS INTERFACE ap_ctrl_none port=return
	static state_type state=s0;
	state_type next_state;##it avoid changing the value of the registers
	bool door_open_local=0;
	
	
	switch(state){
case s0:
	if (x==2)
	{next_state=s2;}
	else{next_state=s0;}
door_open_local=0;
break;
case s2:
	break;
	default :
	break;
state=next_state;
door_open=door_open_local;
	


TIMER 

TO DO THE TASK AFTER A PREDEFINED AMOUNT OF TIME ON SEVERAL OCAASION
IT HAS START ,n,clock,end .
T=10ns,
n=1000000
then the timing interval is nT.
timing interval is 10 ms.

#include "timer.h"




typedef enum{idle, running} timer_state_type;
void timer(ap_uint<N> n, bool start, bool &end) {
	#pragma HLS INTERFACE ap_none port=start
	#pragma HLS INTERFACE ap_none port=end
	#pragma HLS INTERFACE ap_none port=n
	#pragma HLS INTERFACE ap_ctrl_none port=return

	static timer_state_type state = idle;
	static unsigned long long int timer_variable = 0;


	timer_state_type       next_state;
	unsigned long long int next_timer_variable;

	bool end_local;

	switch(state) {
	case idle:
		if (start == 1) {
			next_state     = running;
			end_local = 0;
			next_timer_variable = 0;
		} else {
			next_state     = idle;
			end_local = 0;
			next_timer_variable = 0;
		}

		break;
	case running:

		if (timer_variable == n-1) {
			next_state          = idle;
			end_local           = 1;
			next_timer_variable = 0;
		} else {
			next_timer_variable = timer_variable + 1;
			next_state          = running;
			end_local           = 0;
		}
		break;
	default:
		break;
	}

	state          = next_state;
	timer_variable = next_timer_variable;
	end            = end_local;


}



COUNTER 
inputs can be 
1.Reset 
2.Enable
3.Direction
4.Data.
5.Load


Clock divider
ffast=fslow/N

#include "clock_generator.h"

#define CLOCK_FREQUENCY_DIVISOR  20

typedef enum{zero, one} clock_state_type;

void clock_generator(bool &slow_clock_signal) {
#pragma HLS INTERFACE ap_none port=slow_clock_signal
#pragma HLS INTERFACE ap_ctrl_none port=return



	static clock_state_type state = zero;
	static unsigned int     counter = CLOCK_FREQUENCY_DIVISOR /2-1;

	clock_state_type next_state;
	unsigned int     next_counter;

	bool slow_clock_signal_local;

	switch(state) {
	case zero:
		if (counter == 0) {
			next_counter       = CLOCK_FREQUENCY_DIVISOR /2-1;
			next_state         = one;
		} else {
			next_counter       = counter-1;
			next_state         = zero;
		}
		slow_clock_signal_local = 0;
		break;
	case one:
		if (counter == 0) {
			next_counter       = CLOCK_FREQUENCY_DIVISOR /2-1;
			next_state         = zero;
		} else {
			next_counter       = counter-1;
			next_state         = one;
		}
		slow_clock_signal_local = 1;
		break;
	default:
		break;
	}

	state = next_state;
	counter = next_counter;
	slow_clock_signal = slow_clock_signal_local;

}



##PULSE GENERATOR 


HERE CONSIDER A CLOCK AND THE DATA THEN THE OUPTUT IS THE SINGLE CYCLE NARROW PULSE 
THE PROPSED PULSE GENERATOR HAS TWO STATES WAIT FOR ONE HERE THE SIGNAL IS ONE  AND WAIT FOR ZERO HERE THE SIGNAL IS ZERO  .







##SINGLE CYCLE REGULAR PULSES


IT TAKES A CLOCK SIGNAL TC AND GENRATE A CLOCK CYCLE WITH A SIGNAL CONTAINING ONLY ONE PULSE 

n denotes the number of clcok cycles that ifts in the period of the output clock signal 
n=Tp/tc
output signal 

EDGE DETECTOR 


HEWRE THE INPUT IS THE INPUT SIGNAL AND THE CLOCK SIGNAL A SINGLE CYCLE PULSE IS ASSIGNED TO EACH OUTPUT I.E THE RISING AND THE FALLING EDGE WHEN THE INPTU GOES HIGH OR LOW



INTEGRATED LOGIC ANALYSER 

TO DEBUG A DESIGN.SINCE PHYSICAL LOGIC ANALYSER MAY NOT BE AVAILABLE TO COPE WITH THIS SITUATION WE USE THE ILA . 

SIGNAL IN AN FPGA DESIGN CAN BE CONNECTED TO AN ILA THESE INPUT ATACHED TO A PROBE INPUT ARE SAMPLED AT THE DESIGN SPEED AND STORED USING ONCHIP BLOCK RAM USING BRAM .
 THE CORE PARAMETERE SPECIFIES THE NUMBER OF PROBE ,TRACE SAMPLE,WIDHT OF EACH PROBE INPUT .
 COMMUNICATION WIHT ILA IS DONE WITH AN AUTOINSTANTIATED DEBUG CORE HUB THAT CONNECT TO THE JTAG INTEFACE OF THE FPGA.
 ILA NEED CLOCK TO SYNCHRONIZE ITS DESIGN FUNCTIONALITY AND DATA SMAPLIJNG WITH THE DESGIN UNDER TEST .
 A SET OF CUSTOMIZABLE PORTS COMES UNDER DESIGN UNDER INSPECTION 
 ILA CAN PROVIDE UPTO 1024 PROBES AND EACH PROBES HAVE UPTO 4096 BITS 
 EACH PROBE CAN BE CUSTOMIZED A STHE DATA TRIGGER AND THE PORT 
 
 IT HAS
 1. SAMPLING CIRCUIT:-SAMPLES THE DATA IN THE PROBE 
 2.BRAM :STORES THE SAMPLES IN THE BRAM (SIZE OF BRAM IS DEFINED BY SAMPLE DATA DEPTH IT DEPEND ON MAXIMUM NUMBER OF SAMPLES THAT CAN BE STORED AT RUN TIME ON EACH PROBE.
 
 ####NUMBER OF SAMPLES DEPEND ON THE BRAM SIZE .
 
 ####MANY PROBE ARE CONNECTED TO TRRIGER THROUGH ALOGIC GATE AND THE TRIGGER IS PUT ONTO THE SAMPLER  



ILA DEBUG FLOW 

1.INSTANTIATION
2. CUSTOMIZATION
3.CONNECTION


SINGLE CYCLE DESIGN APPROACH IS NOT ALWAYS APPLICABLE TO DESIGNING LOGIC CIRCUIT



function PIPELINING

There are sequential tasks in which one task depend on the other and only after one is completed other gets started 
THUS THEY CANNOT PERFORM IN THE FULL PARRALLEL FASHION

ON WAY TO EXECUTE THESE TASKS IS THE SEQUENTIAL EXECUTION WHERE ONE DATA WILL FIRST COMPLETE THE TASK AND THEN GO TO THE OTHER 
another execution is pipelining in which tasks execution can overlap 

here task 1 recevie the first data and after task 1 goes to task 2 then task 1 willl process the second data.
while task 3 process the first data task 2 process the second data .


PIPELINING IS APPLIED TO FUUCNTIONS AND LOOPS 

FOR COMBINATIONAL CIRCUIT TO BE EXECUTED IN TWO CLOCK CYCLE IT IS DIVIDED INTO TWO PARTS AND EACH PART IS RUN IN THE ONE CYCLE .
IN THIS CASE THE INTIIALTION INTERVAL IS ONE.


FUNCITON PIPELINING IN HLS 


##pragma HLS PIPELINE is used for pipelining 


Performance Metric 

1:Intiaiton interval:-the speed that the cicuit can represent new inputs  ,
2:latency:-timing between an input and its corresponding output 
3:Throughput:- Thespeed of genrating output data

Single clock cycle Initiation interval (SCII):-the circuit can accepts input adn genrate data ineach clock cycle 



PMOD 

PERIPHERAL MODULE INTERFACE 
IT IS USED TO CONNECT LOW FREQUENCY 
LOW I/O PINS COUNT PERIOPHERAL MODULE
IT COMMUNICATE WITH THE BOARD USING 6 12 OR 8 PIN CONNECTORS THAT CAN CARRY MULTIPLE DIGITAL CONTROL SIGNAL INCLUDING SPI AND OTHER SERIAL PROTOCOL

SCII design flow pros and cons 

SCII design approach provides efficiretn implementation int he sequential cirucit such that they can accept inputs in every clock cycle .
Provides the maximum throughput available through an application 
SCII benefits 

1. Performance :high performance as there is no wastage of the clock cycle 
2.Simplicity of the communcaiton between two hardare module:If M1 genratee data in every clock cycle than M2 genrate data in the following clock cycle
Thus there is no need to have any synchronization mechanism between them 


SCII drawback 

1. uncsyncronized communciation 
2.high clock period


HANDSHAKKING MECHANISM AVAILABLE IN HLS AND HOW CAN WE USE THEM 

HARDWARE MODULE COMMUNCAITON 
HARDWARE MODULE USED PORTS TO COMMUNICATE TOGEHTER .
THE NUMBER OF SIGNAL AND THERE CHARACTERSITICS ARE DEFINED AST HE PORTS INTERFACE 
THE ASSOCIATED PROTOOCOL DEFINE the mechanism of data transaction on the corresponding port
protocol defines the event sequences of the synchronizationmechanism between two connected ports.

PORT INTERFACE AND PROTOCOL CONCEPT ARE VERY CLOSELY RELATED .


HLS AND HARDWARE MODULE 
TOP FUINCITON OF AN HLS MODULE IS SYNTHESIZED INTO AN HARDAWARE MODULE.
ALSO ITS RETURN VALUE IS ASSINGED TO OPUTPUT AND CALL BY REFERENCE IS ASSIGNED TO INPUT AND OUTPUT AND CALL BY VALUE OIS ASSIGNED TO INPUT
CALL BY VALUE ARGUMENT ARE ASSIGNED TO INPUT AND 

ARRAY AND POINTERS NEED ACCESIGN TO MEEMORIES SO THEY NEED SPECIAL ATTENTION

COMMUNCATION BETWEEN MODULES DONOT REQUIRE ANY SYNCHRONIZATION BUT WHEN THE MODULE COMMUNICATE WITH THE PERPHERALS IT REQUIRE THE SYNCHRONIZATION.


COMMUNICATION AMONG MODULES INSIDE THE FPGA AND COMMUNCATION AMONG MODULES OUTSIDE THE FPGA .

WHAT IS THE BASIC IDEA OF COMUINICATION BETWEEN THE TWO HARDWARE MODLES 



TWO MODULES CAN PROPERLY COMMUNICATE IF THEY ARE READY FOR DATA EXCHANGE  AND ARE AWARE OF THESE TIMING .
IT IS USUALY CALLED SYNCHRONIZATION

INTERFACE SYNTHESIS

VITIS HLS CREATES THREE TYPES OF PORTS:
1.CLOCK AND REST PORTS 
2. BLOCK LEVEL PORTS 
3.ARGUMENT LEVEL PORTS

IF THE DESIGN TAKE SMORE THAN ONE CYCLE TO COMPLETE ITS OPERATION OR HAS A MEMEORY CELL THAN THE SYNTHESSIS TOOL ADDS CLOCK AND REST TO THE RTL 

RESET IS USED TO RESTORE THE VALUE IN FPGA OR BLOCK RAM TO THE INTIAL VALUE 
if avariable is static or global the reset pragma is used to explicity add a reset or the variable can be removed from the reset by turning of the pragma .
#pragma HLS reset variable=<a> off

CLOCK ENABLE(CHIP ENABLE ) CAN BE ADDED TO THE ENTIORE BLOCK USING SOLUTION SOLUTION SETTINGS GENRAL AND CONFIG INTERFACE USING 
IT PRVENT ALL CLOCK OPERATION WHEN IT IS ACTIVE LOW


BY DEFAULT A BLOCK LEVEL INTERFACE PROTOCOL IS ADDDE TO A DESIGN THESE SIGNAL CONTROL THE BLOCK INDEPENDTLY OF ANY I/O SIGNAL PROTOCOL

AP_START:WHEN THE BLOCK CAN START PROCESSING DATA 
AP_DONE:OR HAS COMPLETED THE OPERATION 
AP_IDLE:INDICATE WHEN THE DESIGN IS IDLE 
AP_READY:WHEN IT IS READY TO ACCEPT NEW INPUT
POINTER O IS SYNTHESIZED INTO AN INPUT PORT AND AN OUTPUT PORT AND THE VALIDITY SIGNALS.



HOW TO READ THE STATUS OF MODULE OR COMMAND THAT TO START ITS TASK 

BLOCK LEVEL INTERFACE PROTOCOL IS ADDED TO THE DESIGN BY DEFAULT 
PORT OF THE BLOCK LEVEL
AP_START:WHEN THE BLOCK CAN START PROCESSING DATA 
AP_DONE:OR HAS COMPLETED THE OPERATION 
AP_IDLE:INDICATE WHEN THE DESIGN IS IDLE 
AP_READY:WHEN IT IS READY TO ACCEPT NEW INPUT

BLOCK LEVEL INTERFACE TYPES 


ap_ctrl_none
ap_ctrl_hs
ap_ctrl_chain
s_axilite
Block level handshake signal specify the following 
when the design can start to perform the operation
when the operations end
when the design is idle and ready for the nnew inputs 

#pragma HLS INTERACE ap_ctrl_hs port =return
it adds the return at the output of the IP 

block level interfacing through a multicycle example 

BLOCK LEVEL (ap_hs)-vitis HLS

####for connecting the two ip connect the ap _start of the one to the ap_done of the next .



##CHECK THE VALIDITY OF THE DATA VALUE ON A PORT 
AP_VLD PORT LEVEL INTERFACE 

AP_NONE PORT INTERFACE IMPLEMENT THE PORT WITH SIMPLE WIRES AND IT IS THE DESIGNERS REPONSIBLITY THAT THE PORT DATA VALUES ARE VALID AT THE TIME OF SAMPLING AND USSAGE 
BUT IN MULTICYCLE DESIGN IT IS NOT POSSIBLE SO IT IS GOOD TO ATTACH A VALIDITY SIGNAL TO CHECK IF THE PORT DATA VALUE IS VALID FOR SAMPLING

##AP_VLD HELP US TO ATTACH SUCH KIND OF SIGNAAL TO EACH PORT 
##AT THE INPUT PORT THE DESIGN WILL CHECK THE VALID SIGNAL IF ITS ACTIVE THEN THE INPUT DATA CAN BE SAMPLED 
##AT THE OUTPUT AS SOON AS THE DESIGN ASSIGN THE PROPER VALUE TO THE OUTPUT PORT IT ALSO ACTIVATES THE CORRESPONDING VALID SIGNAL 


##HOW TO DESIGN THAT THE SEND DATA IS USSED IN ACOMMMUNCARTION PROCESS
## AT SOME TIME IT IS BETTER THAT THE GNERATOR KEEPS THE DATA VALID UNTILLL IT ENSURES THAT THE CONSUMER RECEIVED THE DATA 
##AT THIS CASE IT CAN WAIT TO RECEIVE AN ACKNOWLEDGE SIGNAL FROM THE RECEIVER
##AP_ACK IS USED FOR SUCH CASES IT CAN BE APPLIED FOR BOTH INPUT AND THE OUTPUT 

##FOR INPUT ARGUMENT THE DEIGN GENRATES AN OUTPUT ACKNOWLEDGE TO CONFIRM THAT IS ACTIVE HIGH IN THE CYCLE WHEN THE INPUT IS READ 
##FOR OUTPUT ARGUMENT VITIS HLS IMPLEMEMENT AN INPUT ACKNOWLEDGE PORT PORT TO CONFIRMS THE OUTPUT WAS READ .

##AP_HLS AND THE AP_ACK PROVIDE ONE WAY HANDSHAKING BETWEEN TWO MODULOE HOW CAN WE PROVIDE TWO WAY HANDSHAKING BETWEEN THE TWO MODULES.

##HOW CAN WE PROVIDE TWO WAY HANDSHAKING IN VITIS HLS 
ap_hs provide th efollowing signal data port and acknowledge signal to indicate when data is consumed and valid signal to indicate when data is read 


UNDER A TWO WAY HANDSHAKING MECHNISM WHEN A MODULE IS GOIND TO RECEIVE DATA IT WAITS TO RECEIVE A VALID SIGNAL ON THE DESIRED INPUT PORT WHEN IT RECEIVE THE VALID SIGNAL IT SENDS THE 
ACKNOELEDGE SIGNAL TO THE SENDER MODULE

WHEN A MODULE ASSIGN DATA ON APORT WITH AN API_HS INTERFACE IT ACTIVATES THE VALID SIGNAL AND WILL WAIT FOR THE ACKNOPWLEDGE AFTER IT ACTIVATES THE VALID SIGNAL AND RELEASES THE PORT DATA 
OR ASSIGN NEW DATA .

SWITCH DEBOUNCING I.E AFTER SWITCH GETS ON THERE ARE MANY TRANSITION SO TO DEAL WITH IT AT A PARTICULAR CLOCK CYCLE RISING EDGE WE START SAMPLING ADN AFTER PARTICULAR TIME
WE AGAIN SAMPLE TO GET THE PULSE WITH clEAR edge.



If the sampling pulse delay is greater than the bouncing period then the circuit can genrate the desired output .

void find(char val,char in_vec[10],char out_vec[10])
array on the top infer function infer RAM PORTS AND FIFO PORTS.
IN_VEC_ADDRESS,IN_VEC_CE,IN_VEC_QO. OUT_VEC_ADDRSSS,OUT_VEC_CE,OUT_VEC_WE,OUT_VEC_DO.
					 
					 




















