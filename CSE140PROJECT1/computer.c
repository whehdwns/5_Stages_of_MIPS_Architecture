//whehdwns
#include <stdio.h>
#include <stdlib.h>
#include <netinet/in.h>
#include "computer.h"
#undef mips			/* gcc already has a def for mips */

unsigned int endianSwap(unsigned int);

void PrintInfo (int changedReg, int changedMem);
unsigned int Fetch (int);
void Decode (unsigned int, DecodedInstr*, RegVals*);
int Execute (DecodedInstr*, RegVals*);
int Mem(DecodedInstr*, int, int *);
void RegWrite(DecodedInstr*, int, int *);
void UpdatePC(DecodedInstr*, int);
void PrintInstruction (DecodedInstr*);

/*Globally accessible Computer variable*/
Computer mips;
RegVals rVals;

/*
 *  Return an initialized computer with the stack pointer set to the
 *  address of the end of data memory, the remaining registers initialized
 *  to zero, and the instructions read from the given file.
 *  The other arguments govern how the program interacts with the user.
 */
void InitComputer (FILE* filein, int printingRegisters, int printingMemory,
  int debugging, int interactive) {
    int k;
    unsigned int instr;

    /* Initialize registers and memory */

    for (k=0; k<32; k++) {
        mips.registers[k] = 0;
    }

    /* stack pointer - Initialize to highest address of data segment */
    mips.registers[29] = 0x00400000 + (MAXNUMINSTRS+MAXNUMDATA)*4;

    for (k=0; k<MAXNUMINSTRS+MAXNUMDATA; k++) {
        mips.memory[k] = 0;
    }

    k = 0;
    while (fread(&instr, 4, 1, filein)) {
	/*swap to big endian, convert to host byte order. Ignore this.*/
        mips.memory[k] = ntohl(endianSwap(instr));
        k++;
        if (k>MAXNUMINSTRS) {
            fprintf (stderr, "Program too big.\n");
            exit (1);
        }
    }

    mips.printingRegisters = printingRegisters;
    mips.printingMemory = printingMemory;
    mips.interactive = interactive;
    mips.debugging = debugging;
}

unsigned int endianSwap(unsigned int i) {
    return (i>>24)|(i>>8&0x0000ff00)|(i<<8&0x00ff0000)|(i<<24);
}

/*
 *  Run the simulation.
 */
void Simulate () {
    char s[40];  /* used for handling interactive input */
    unsigned int instr;
    int changedReg=-1, changedMem=-1, val;
    DecodedInstr d;

    /* Initialize the PC to the start of the code section */
    mips.pc = 0x00400000;
    while (1) {
        if (mips.interactive) {
            printf ("> ");
            fgets (s,sizeof(s),stdin);
            if (s[0] == 'q') {
                return;
            }
        }

        /* Fetch instr at mips.pc, returning it in instr */
        instr = Fetch (mips.pc);

        printf ("Executing instruction at %8.8x: %8.8x\n", mips.pc, instr);

        /*
	 * Decode instr, putting decoded instr in d
	 * Note that we reuse the d struct for each instruction.
	 */
        Decode (instr, &d, &rVals);

        /*Print decoded instruction*/
        PrintInstruction(&d);

        /*
	 * Perform computation needed to execute d, returning computed value
	 * in val
	 */
        val = Execute(&d, &rVals);

	UpdatePC(&d,val);

        /*
	 * Perform memory load or store. Place the
	 * address of any updated memory in *changedMem,
	 * otherwise put -1 in *changedMem.
	 * Return any memory value that is read, otherwise return -1.
         */
        val = Mem(&d, val, &changedMem);

        /*
	 * Write back to register. If the instruction modified a register--
	 * (including jal, which modifies $ra) --
         * put the index of the modified register in *changedReg,
         * otherwise put -1 in *changedReg.
         */
        RegWrite(&d, val, &changedReg);

        PrintInfo (changedReg, changedMem);
    }
}

/*
 *  Print relevant information about the state of the computer.
 *  changedReg is the index of the register changed by the instruction
 *  being simulated, otherwise -1.
 *  changedMem is the address of the memory location changed by the
 *  simulated instruction, otherwise -1.
 *  Previously initialized flags indicate whether to print all the
 *  registers or just the one that changed, and whether to print
 *  all the nonzero memory or just the memory location that changed.
 */
void PrintInfo ( int changedReg, int changedMem) {
    int k, addr;
    printf ("New pc = %8.8x\n", mips.pc);
    if (!mips.printingRegisters && changedReg == -1) {
        printf ("No register was updated.\n");
    } else if (!mips.printingRegisters) {
        printf ("Updated r%2.2d to %8.8x\n",
        changedReg, mips.registers[changedReg]);
    } else {
        for (k=0; k<32; k++) {
            printf ("r%2.2d: %8.8x  ", k, mips.registers[k]);
            if ((k+1)%4 == 0) {
                printf ("\n");
            }
        }
    }
    if (!mips.printingMemory && changedMem == -1) {
        printf ("No memory location was updated.\n");
    } else if (!mips.printingMemory) {
        printf ("Updated memory at address %8.8x to %8.8x\n",
        changedMem, Fetch (changedMem));
    } else {
        printf ("Nonzero memory\n");
        printf ("ADDR	  CONTENTS\n");
        for (addr = 0x00400000+4*MAXNUMINSTRS;
             addr < 0x00400000+4*(MAXNUMINSTRS+MAXNUMDATA);
             addr = addr+4) {
            if (Fetch (addr) != 0) {
                printf ("%8.8x  %8.8x\n", addr, Fetch (addr));
            }
        }
    }
}

/*
 *  Return the contents of memory at the given address. Simulates
 *  instruction fetch.
 */
unsigned int Fetch ( int addr) {
    return mips.memory[(addr-0x00400000)/4];
}

void immediate(DecodedInstr *d){//1 if sign immediate , 0 if the zero immediate
    if(1 == d->regs.i.addr_or_immed >> 15){ //SignExtImm = { 16{immediate[15]}, immediate }
            d->regs.i.addr_or_immed = (0xffff0000) | (d->regs.i.addr_or_immed);
            //0xffff0000= 4294901760,  2^32=4294967296
        }
        else{//ZeroExtImm = { 16{1b’0}, immediate }
            d->regs.i.addr_or_immed = (0x0000FFFF & (d->regs.i.addr_or_immed));
            //0x0000FFFF 65525,  2^16=65536
        }
}


void Rformat(unsigned int instr, DecodedInstr* d, RegVals* rVals){
            /*d->regs.r.rd = instr<<16 >> 27;
            d->regs.r.shamt = instr <<21 >> 27;
            d->regs.r.funct = instr <<26 >> 26;
            rVals->R_rs = mips.registers[d->regs.r.rs = instr <<6 >> 27];
            rVals->R_rt = mips.registers[d->regs.r.rt = instr <<11  >> 27];
            rVals->R_rd=0;
            */
            d->regs.r.rd = (instr & 0x0000F800) >> 11;
            d->regs.r.shamt = instr <<21 >> 27;
            d->regs.r.funct = instr <<26 >> 26;
            rVals->R_rs = mips.registers[d->regs.r.rs = instr <<6 >> 27];
            rVals->R_rt = mips.registers[d->regs.r.rt = instr <<11  >> 27];
            rVals->R_rd=0;
}

void iformat(unsigned int instr, DecodedInstr* d, RegVals* rVals){
            d->regs.i.addr_or_immed = instr << 16 >> 16;
            rVals->R_rs = mips.registers[d->regs.i.rs = instr << 6 >> 27];
            rVals->R_rt = mips.registers[d->regs.i.rt = instr << 11 >> 27];
            immediate(d);
}


/* Decode instr, returning decoded instruction. */
void Decode ( unsigned int instr, DecodedInstr* d, RegVals* rVals) {
    /* Your code goes here */
    //unsigned int  op, funct, shamt, rs, rt, rd, address, immed;
    //int jal, j, jr, addu, addiu, subu, sll, srl, and, andi, or, ori, lui, slt, beq, bne, lw, sw;
  /*  #jump and link, jump to address, jump to register, add unsigned, add unsigned immediate, unsigned subtract, shiftleft, shift right
    #and, and immediate, or, ori(loads lower 16 bits), load upper immediate, set less than, branch equal, branch not eqaul, load word, store word.

    #R type= Opcode(6) 31-26, rs(5) 25-21, rt(5) 20-16, rd(5) 15-11, shamt(5) 10-6 , funct(6) 5-0
    #I type=Opcode(6), 31-26 rs(5) 25-21, rt(5) 20-16, immediate(16) 16-0
    #J type= Opcode (6) 31-26, address (26) 25-0
    #32-opcode(6)=26*/
    d ->op= instr >> 26;
   /* unsigned ops = 0xFC000000;
    unsigned rsinstr = 0x03E00000;
    unsigned rtinstr = 0x001F0000;
    unsigned rdinstr = 0x0000F800;
    unsigned shamtinstr = 0x000007C0;
    unsigned functinstr = 0x0000003F;
    jaddress 0x03ffffff;
*/
    switch(d->op){
        //R type
        case 0: {
            Rformat(instr,d,rVals);
            break;}
        //J format
        case 0x2:
        case 0x3:
            //J format, removes 4 bits and shift right 2.
            //From 2^16, they shift right by 2*(2) to make 2^18, 262144
                {
                    d->regs.j.target =instr<< 6 >>4;
                     //2^26-1
                    //d->regs.j.target =instr & 0x03ffffff;

                    break;}

        //I format
        case 0x9: //addiu
        case 0xc://andi
        case 0xd://ori
        case 0xf://lui
        case 0x4://beq
        case 0x5://bne
        case 0x23://lw
        case 0x2b://sw
            iformat(instr,d,rVals);
            break;


    }

}



/*
 *  Print the disassembled version of the given instruction
 *  followed by a newline.
 */
void PrintInstruction ( DecodedInstr* d) {
    /* Your code goes here */
     int rd = d->regs.r.rd;
    int rs = d->regs.r.rs;
    int rt = d->regs.r.rt;
    int jadd=d->regs.j.target;
    int shamtt=d->regs.r.shamt;
    int rsi = d->regs.i.rs;
	int rti = d->regs.i.rt;
    int immm=d->regs.i.addr_or_immed;
    //char* instr = (char*) malloc(6*sizeof(char));
    switch(d->op) {
            //R format
            case 0x0:{
			switch ((d->regs.r.funct)   ) {

                case 0x00:{  printf("sll\t$%u, $%u, $%d \n" ,rd, rt, shamtt); return;}//sll $rd, $rt, shamt
                case 0x02:{ printf("srl\t$%u, $%u, $%d \n",rd, rt, shamtt); return;}//srl $rd, $rt, shamt
                case 0x8:{ printf("jr\t$31\n"); return;}//jr $rs
                 case 0x21:{printf("addu\t$%u, $%u, $%u \n", rd, rs, rt); return;}//addu $rd, $rs, $rt
                case 0x23:{ printf("subu\t$%u, $%u, $%u \n", rd, rs, rt); return;}//subu $rd, $rs, $rt
                case 0x24:{ printf("and\t$%u, $%u, $%u \n", rd, rs, rt); return;}//and $rd, $rs, $rt
                case 0x25:{ printf("or\t$%u, $%u, $%u \n", rd, rs, rt); return;}//or $rd, $rs, $rt
                case 0x2a:{printf("slt\t$%u, $%u, $%u \n", rd, rs, rt); return;}//slt $rd, $rs, $rt
                default: break;
            }
        }
        //J format
        case 0x2:{printf("j\t0x%8.8x \n", jadd); return;}//j address
        case 0x3:{printf("jal\t0x%8.8x \n",jadd); return;}//jal address

        //I format
        case 0X9:{printf("addiu\t$%u, $%u, %d \n",rti, rsi, immm); return;}//addiu $rt, $rs, imm
        case 0xc:{printf("andi\t$%u, $%u, 0x%x \n",rti, rsi, immm);return;}//andi $rt, $rs, imm
        case 0xd:{printf("ori\t$%u, $%u, 0x%x \n",rti, rsi,immm);return;}//ori  $rt, $rs, imm
        case 0xf:{printf("lui\t$%u, 0x%x \n",rti, immm<<16);return;}//lui  $rt, imm     {(imm)[15:0], 0 × 16}
        case 0x4:{printf("beq\t$%u, $%u, 0x%8.8x \n", rsi, rti, mips.pc + 4 + (immm << 2) ); return;}//beq $rs, $rt, imm
        case 0x5:{printf("bne\t$%u, $%u, 0x%8.8x \n", rsi, rti, mips.pc + 4 + (immm << 2) ); return;}//bne $rs, $rt, imm
        case 0x23:{printf("lw\t$%u, %d(29)\n", rti, immm); return;}//lw $rt, imm($rs)
        case 0x2b:{printf("sw\t$%u, %d(29)\n", rti, immm); return;}//sw $rt, imm($rs)
        default: exit(1);
    }

}

/* Perform computation needed to execute d, returning computed value */
int Execute ( DecodedInstr* d, RegVals* rVals) {
    /* Your code goes here */
    int rd = d->regs.r.rd;
    int rs = d->regs.r.rs;
    int rt = d->regs.r.rt;
    int shamtt=d->regs.r.shamt;
    int regvlf=d->regs.r.funct;

    int jadd=d->regs.j.target;

    int rsi = d->regs.i.rs;
	int rti = d->regs.i.rt;
    int immm=d->regs.i.addr_or_immed;

    int regvlt=rVals->R_rt;
    int regvls=rVals->R_rs;

    switch(d->op) {
        case 0x0://R format
            switch (regvlf)
            {
                case 0x00://sll => R[$rt] << shamt
                    {return regvlt << shamtt;
                        break;}
                case 0x02://srl =>R[$rt] >> shamt
                    {return regvlt >> shamtt;
                        break;}
                case 0x8:// jr => R[$rs]	R[$rs] must be a multiple of 4
                    {return mips.registers[31];
                        break;}
                case 0x21://addu =>R[$rs] + R[$rt]
                    {return regvls + regvlt;
                        break;}
                case 0x23://subu =>R[$rs] - R[$rt]
                    {return regvls - regvlt;
                        break;}
                case 0x24://and =>  R[$rs] & R[$rt]
                    {return regvls & regvlt;
                        break;}
                case 0x25://or => R[$rs] | R[$rt]
                    {return regvls | regvlt;
                        break;}
                case 0x2a://slt => R[$rs] < R[$rt]
                    { if(regvls < regvlt) //slt
                                return 1;
                            else
                                return 0;
                            break;}
            }
            //J format
            case 0x2://pc=pc_upper|(target<<2)
                {return ((mips.pc >> 16 <<16) | d->regs.j.target);
                    break;}
            case 0x3:/*jal => {(PC + 4)[31:28], address, 00}*/ //  r31=pc; pc=target<<2
                {return mips.pc += 4;
                    break;}
           //I format
                case 0x4:{/*beq=> if(R[$rs] = R[$rt]) PC ← PC + 4 + SignExt18b({imm, 00})*/
                    if(mips.registers[rsi]==mips.registers[rti]){
                        return 1;
                    }else{
                        return 0;}
                        break;}
                case 0x5:{/*bne => if(R[$rs] != R[$rt]) PC ← PC + 4 + SignExt18b({imm, 00})*/
                 if(mips.registers[rsi]!=mips.registers[rti]){
                    return 1;
                 }else{
                    return 0;}
                 break;}

                case 0x9:/*addiu=> R[$rs] + SignExt16b(imm)*/
                    {return regvls + immm;
                        break;}
                case 0xc:/*andi =>  R[$rs] & {0 × 16, imm}*/
                    {return regvls & immm;
                        break;}
                case 0xd:/*ori => R[$rs] | {0 × 16, imm}*/
                    {return regvls | immm;
                        break;}
                case 0xf:/*lui => {(imm)[15:0], 0 × 16}*/
                    {return immm << 16;
                        break;}
                case 0x23:/* lw => R[$rt] ← Mem4B(R[$rs] + SignExt16b(imm))*/
                    {return immm + mips.registers[rsi];
                        break;}
                case 0x2b:/*sw => Mem4B(R[$rs] + SignExt16b(imm)) ← R[$rt]*/
                    {return immm + mips.registers[rsi];
                        break;}
                default: {return 0; break;}
    }
  return 0;
}

/*
 * Update the program counter based on the current instruction. For
 * instructions other than branches and jumps, for example, the PC
 * increments by 4 (which we have provided).
 */
void UpdatePC ( DecodedInstr* d, int val) {
    //j, jal, bne,, beq
   mips.pc+=4;//PC increments by 4
    if(d->regs.r.funct == 0x8 && d->op == 0x0){   // jr and R format
        mips.pc = mips.registers[31];
    }//jr
    switch(d->op){
        case 0x2://j pc=pc_upper|(target<<2)
            {mips.pc = val;
                break;}
        case 0x3://jal  r31=pc; pc=target<<2
            {mips.pc = d->regs.j.target;
                break;}
        case 0x4:{//beq if(R[$rs] = R[$rt]) PC ← PC + 4 + SignExt18b({imm, 00})
            if(val == 1){
                    mips.pc += d->regs.i.addr_or_immed<<2;
            }else {
                mips.pc = mips.pc;}
            break;}
        case 0x5:{//ben if(R[$rs] != R[$rt]) PC ← PC + 4 + SignExt18b({imm, 00})
            if(val == 1){
                    mips.pc += d->regs.i.addr_or_immed<<2;
            }else {
                mips.pc = mips.pc;}
            break;}

            default: return;
    }
    /* Your code goes here */
}

/*
 * Perform memory load or store. Place the address of any updated memory
 * in *changedMem, otherwise put -1 in *changedMem. Return any memory value
 * that is read, otherwise return -1.
 *
 * Remember that we're mapping MIPS addresses to indices in the mips.memory
 * array. mips.memory[0] corresponds with address 0x00400000, mips.memory[1]
 * with address 0x00400004, and so forth.
 *
 */
int Mem( DecodedInstr* d, int val, int *changedMem) {
  /* #lw and sw*/
   /* Your code goes here */
//mips.memory[(addr-0x00400000)/4]; from fetch
  //Default to no changes
  *changedMem = -1;
  if ((d->op == 0x23 || d->op == 0x2b) && (val < 0x00401000 || val > 0x00403fff )) {
		printf("Memory Access Exception at 0x%.8x: address 0x%.8x", mips.pc - 4, val);
		exit(0);
  }
  switch(d->op){
        case 0x23://lw  R[$rt] ← Mem4B(R[$rs] + SignExt16b(imm))
            {return mips.memory[(val-0x00401000)/4];
         break;}
        case 0x2b://sw Mem4B(R[$rs] + SignExt16b(imm)) ← R[$rt]
            {*changedMem = val;
            mips.memory[(val - 0x00401000)/4] = mips.registers[d->regs.i.rt];
            return 0;
            break;}
    default: return val;
}
}



/*
 * Write back to register. If the instruction modified a register--
 * (including jal, which modifies $ra) --
 * put the index of the modified register in *changedReg,
 * otherwise put -1 in *changedReg.
 */
void RegWrite( DecodedInstr* d, int val, int *changedReg) {
    *changedReg = -1;
    // if(d->regs.r.funct == 0x8 && d->op == 0x0){
      //  return;
    //}
    switch (d->op) {
         case 0x0:
			switch ((d->regs.r.funct)) {
			    case 0x00://sll
				case 0x02://srl
				case 0x21://addu
				case 0x23://subu
				case 0x24://and
				case 0x25://or
				case 0x2a://slt
                        *changedReg = d->regs.r.rd;
                        mips.registers[d->regs.r.rd] = val;
                        return;
			} return;

		//case 0x2://j
        case 0x3://jal
			*changedReg = 31; //31~0
			mips.registers[31] = val;
			return;
       /*# case Iformat:*/
            case 0x9://addiu
            case 0xc://andi
            case 0xd://ori
            case 0xf://lui
            case 0x23://lw
                    *changedReg = d->regs.i.rt;
                    mips.registers[d->regs.i.rt] = val;
			return;

	}
    /* Your code goes here */

    }
