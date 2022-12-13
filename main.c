
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <stdint.h>


enum opcode_decode {R = 0x33, I = 0x13, S = 0x23, L = 0x03, B = 0x63, JALR = 0x67, JAL = 0x6F, AUIPC = 0x17, LUI = 0x37};

typedef struct {
    size_t data_mem_size_;
    uint32_t regfile_[32];
    uint32_t pc_;
    uint8_t* instr_mem_;
    uint8_t* data_mem_;
} CPU;

void CPU_open_instruction_mem(CPU* cpu, const char* filename);
void CPU_load_data_mem(CPU* cpu, const char* filename);

CPU* CPU_init(const char* path_to_inst_mem, const char* path_to_data_mem) {
	CPU* cpu = (CPU*) malloc(sizeof(CPU));
	cpu->data_mem_size_ = 0x400000;
    cpu->pc_ = 0x0;
    CPU_open_instruction_mem(cpu, path_to_inst_mem);
    CPU_load_data_mem(cpu, path_to_data_mem);
    return cpu;
}

void CPU_open_instruction_mem(CPU* cpu, const char* filename) {
	uint32_t  instr_mem_size;
	FILE* input_file = fopen(filename, "r");
	if (!input_file) {
			printf("no input\n");
			exit(EXIT_FAILURE);
	}
	struct stat sb;
	if (stat(filename, &sb) == -1) {
			printf("error stat\n");
			perror("stat");
		    exit(EXIT_FAILURE);
	}
	printf("size of instruction memory: %d Byte\n\n",sb.st_size);
	instr_mem_size =  sb.st_size;
	cpu->instr_mem_ = malloc(instr_mem_size);
	fread(cpu->instr_mem_, sb.st_size, 1, input_file);
	fclose(input_file);
	return;
}

void CPU_load_data_mem(CPU* cpu, const char* filename) {
	FILE* input_file = fopen(filename, "r");
	if (!input_file) {
			printf("no input\n");
			exit(EXIT_FAILURE);
	}
	struct stat sb;
	if (stat(filename, &sb) == -1) {
			printf("error stat\n");
			perror("stat");
		    exit(EXIT_FAILURE);
	}
	printf("read data for data memory: %d Byte\n\n",sb.st_size);

    cpu->data_mem_ = malloc(cpu->data_mem_size_);
	fread(cpu->data_mem_, sb.st_size, 1, input_file);
	fclose(input_file);
	return;
}
/**
 * structs for the different instruction types
 */
typedef struct {
	uint8_t rs1;
	uint8_t rs2;
	uint8_t rd;
	uint8_t funct3;
	uint8_t funct7;
} RInstruction;

typedef struct {
	uint8_t rs1;
	uint8_t rd;
	uint8_t funct3;
	uint32_t imm;
} IInstruction;

typedef struct {
	uint8_t rs1;
	uint8_t rs2;
	uint8_t funct3;
	uint32_t imm;
} SInstruction;

typedef struct {
	uint8_t rs1;
	uint8_t rs2;
	uint8_t funct3;
	uint32_t imm;
} BInstruction;

typedef struct {
	uint8_t rd;
	uint32_t imm;
} UInstruction;

typedef struct {
	uint8_t rd;
	uint32_t imm;
} JInstruction;

/**
 * Function to extract the opcode from an instruction
 */
static inline enum opcode_decode decode_opcode(const uint32_t *instruction) {
	enum opcode_decode opcode;
	// read the first 7 Bytes of the instruction
	opcode = (*instruction & 0x7F);
	return opcode;
}

/**
 * Function for extracting register operand values from instructions
 */
enum register_position {RS1, RS2, RD};
uint8_t decode_register(const uint32_t *instruction, const enum register_position reg_pos) {
	uint8_t reg_val;
	switch (reg_pos) {
	case RS1:
		reg_val = (*instruction >> 15) & 0x1f;
		break;
	case RS2:
		reg_val = (*instruction >> 20) & 0x1f;
		break;
	case RD:
		reg_val = (*instruction >> 7) & 0x1f;
		break;
	}
	return reg_val;
}

uint8_t decode_funct3(const uint32_t *instruction) {
	uint8_t funct = (*instruction >> 12) & 0x7;
	return funct;
}

uint8_t decode_funct7(const uint32_t *instruction) {
	uint8_t funct = (*instruction >> 25) & 0x7f;
	return funct;
}
/**
 * R Instructions
 */
RInstruction decode_r_instruction(const uint32_t *instruction) {
	RInstruction r_instr = {
		.rs1 = decode_register(instruction, RS1),
		.rs2 = decode_register(instruction, RS2),
		.rd = decode_register(instruction, RD),
		.funct3 = decode_funct3(instruction),
		.funct7 = decode_funct7(instruction)
	};
	return r_instr;
}

void add(CPU* cpu, const RInstruction *r_instruction) {
	cpu->regfile_[r_instruction->rd] = cpu->regfile_[r_instruction->rs1]
		+ cpu->regfile_[r_instruction->rs2];
	cpu->pc_ += 4;
}

void sub(CPU* cpu, const RInstruction *r_instruction) {
	cpu->regfile_[r_instruction->rd] = cpu->regfile_[r_instruction->rs1]
		- cpu->regfile_[r_instruction->rs2];
	cpu->pc_ += 4;
}

void and(CPU* cpu, const RInstruction *r_instruction) {
	cpu->regfile_[r_instruction->rd] = cpu->regfile_[r_instruction->rs1]
		& cpu->regfile_[r_instruction->rs2];
	cpu->pc_ += 4;
}

void or(CPU* cpu, const RInstruction *r_instruction) {
	cpu->regfile_[r_instruction->rd] = cpu->regfile_[r_instruction->rs1]
		| cpu->regfile_[r_instruction->rs2];
	cpu->pc_ += 4;
}

void xor(CPU* cpu, const RInstruction *r_instruction) {
	cpu->regfile_[r_instruction->rd] = cpu->regfile_[r_instruction->rs1]
		^ cpu->regfile_[r_instruction->rs2];
	cpu->pc_ += 4;
}

void srl(CPU* cpu, const RInstruction* r_instruction) {
	cpu->regfile_[r_instruction->rd] = cpu->regfile_[r_instruction->rs1]
		>> cpu->regfile_[r_instruction->rs2];
	cpu->pc_ += 4;
}

void sra(CPU* cpu, const RInstruction* r_instruction) {
	cpu->regfile_[r_instruction->rd] = (int32_t) cpu->regfile_[r_instruction->rs1]
		>> cpu->regfile_[r_instruction->rs2];
	cpu->pc_ += 4;
}

void sll(CPU* cpu, const RInstruction* r_instruction) {
	cpu->regfile_[r_instruction->rd] = cpu->regfile_[r_instruction->rs1]
		<< cpu->regfile_[r_instruction->rs2];
	cpu->pc_ += 4;
}

void slt(CPU* cpu, const RInstruction* r_instruction) {
	cpu->regfile_[r_instruction->rd] = (int32_t) cpu->regfile_[r_instruction->rs1]
		< (int32_t) cpu->regfile_[r_instruction->rs2];
	cpu->pc_ += 4;
}

void sltu(CPU* cpu, const RInstruction* r_instruction) {
	cpu->regfile_[r_instruction->rd] = cpu->regfile_[r_instruction->rs1]
		< cpu->regfile_[r_instruction->rs2];
	cpu->pc_ += 4;
}

enum funct3_r {
ADD_SUB = 0x0,
SLL = 0x1,
SLT = 0x2,
SLTU = 0x3,
XOR = 0x4,
SRL_SRA = 0x5,
OR = 0x6,
AND = 0x7
};

void execute_r_instruction(CPU* cpu, RInstruction *r_instruction) {
	switch (r_instruction->funct3) {
		case ADD_SUB:
			if (r_instruction->funct7 == 0) {
				add(cpu, r_instruction);
			} else {
				sub(cpu, r_instruction);
			}
			break;
		case AND:
			and(cpu, r_instruction);
			break;
		case OR:
			or(cpu, r_instruction);
			break;
		case XOR:
			xor(cpu, r_instruction);
			break;
		case SRL_SRA:
			if (r_instruction->funct7 == 0) {
				srl(cpu, r_instruction);
			} else {
				sra(cpu, r_instruction);
			}
			break;
		case SLL:
			sll(cpu, r_instruction);
			break;
		case SLT:
			slt(cpu, r_instruction);
			break;
		case SLTU:
			sltu(cpu, r_instruction);
			break;
	}
}

/**
 * I Instructions
 */
uint32_t decode_i_imm(const uint32_t *instruction) {
	uint32_t imm = 0;
	uint8_t extend = (*instruction >> 31) & 1;
	uint16_t imm11_0 = (*instruction >> 20) & 0xFFF;
	imm = (extend * 0xFFFFF000) + imm11_0;

	return imm;
}

IInstruction decode_i_instruction(const uint32_t *instruction) {
	IInstruction i_instr = {
		.rs1 = decode_register(instruction, RS1),
		.rd = decode_register(instruction, RD),
		.funct3 = decode_funct3(instruction),
		.imm = decode_i_imm(instruction)
	};
	return i_instr;
}

/**
 * S Instructions
 */
uint32_t decode_s_imm(const uint32_t *instruction) {
	uint32_t imm = 0;
	uint8_t extend = (*instruction >> 31) & 1;
	uint16_t imm11_5 = (*instruction >> 25) & 0x7F;
	uint8_t imm4_0 = (*instruction >> 7) & 0x1F;
	imm = (0xFFFFF000 * extend) + (imm11_5 << 5) + imm4_0;
	return imm;
}

SInstruction decode_s_instruction(const uint32_t *instruction) {
	SInstruction s_instr = {
		.rs1 = decode_register(instruction, RS1),
		.rs2 = decode_register(instruction, RS2),
		.funct3 = decode_funct3(instruction),
		.imm = decode_s_imm(instruction)
	};
	return s_instr;
}
//TODO check for data_mem_ bounds and mmio (output auf stdout)
void sb(CPU* cpu, const SInstruction* s_instruction) {
	cpu->data_mem_[cpu->regfile_[s_instruction->rs1] + (int32_t) s_instruction->imm]
		= (uint8_t) cpu->regfile_[s_instruction->rs2];
}

//TODO check for data_mem_ bounds and mmio (output auf stdout)
void sh(CPU* cpu, const SInstruction* s_instruction) {
	*(uint16_t*) (cpu->data_mem_ + cpu->regfile_[s_instruction->rs1] + s_instruction->imm)
		= (uint16_t) cpu->regfile_[s_instruction->rs2];
	cpu->pc_ += 4;
}

//TODO check for data_mem_ bounds and mmio (output auf stdout)
void sw(CPU* cpu, const SInstruction* s_instruction) {
	*(uint32_t*) (cpu->data_mem_ + cpu->regfile_[s_instruction->rs1] + s_instruction->imm)
		= (uint32_t) cpu->regfile_[s_instruction->rs2];
	cpu->pc_ += 4;
}

enum funct3_s {
SB = 0x0,
SH = 0x1,
SW = 0x2
};

void execute_s_instruction(CPU* cpu, const SInstruction* s_instruction) {
	switch (s_instruction->funct3) {
		case SB:
			sb(cpu, s_instruction);
			break;
		case SH:
			sh(cpu, s_instruction);
			break;
		case SW:
			sw(cpu, s_instruction);
			break;
	}
}

/**
 * B Instructions
 */
uint32_t decode_b_imm(const uint32_t *instruction) {
	uint32_t imm = 0;
	uint8_t extend = (*instruction >> 31) & 1;
	uint8_t imm12 = extend;
	uint8_t imm11 = (*instruction >> 7) & 1;
	uint8_t imm10_5 = (*instruction >> 25) & 0x3F;
	uint8_t imm4_1 = (*instruction >> 8) & 0xF;

	imm = (extend * 0xFFFFE000)
		+ (imm12 << 12)
		+ (imm11 << 11)
		+ (imm10_5 << 5)
		+ (imm4_1 << 1);
	return imm;
}

BInstruction decode_b_instruction(const uint32_t *instruction) {
	BInstruction b_instr = {
		.rs1 = decode_register(instruction, RS1),
		.rs2 = decode_register(instruction, RS2),
		.funct3 = decode_funct3(instruction),
		.imm = decode_b_imm(instruction)
	};
	return b_instr;
}

void beq(CPU* cpu, const BInstruction *b_instruction) {
	if (cpu->regfile_[b_instruction->rs1] == cpu->regfile_[b_instruction->rs2]) {
		cpu->pc_ = cpu->pc_ + (int32_t) b_instruction->imm;
	} else {
		cpu->pc_ += 4;
	}
}
void bne(CPU* cpu, const BInstruction *b_instruction){
	if (cpu->regfile_[b_instruction->rs1] != cpu->regfile_[b_instruction->rs2]) {
		cpu->pc_ = cpu->pc_ + (int32_t) b_instruction->imm;
	} else {
		cpu->pc_ += 4;
	}
}

/**
 * Signed Comps
 */
void blt(CPU* cpu, const BInstruction *b_instruction){
	if ((int32_t) cpu->regfile_[b_instruction->rs1] < (int32_t) cpu->regfile_[b_instruction->rs2]) {
		cpu->pc_ = cpu->pc_ + (int32_t) b_instruction->imm;
	} else {
		cpu->pc_ += 4;
	}
}

void bge(CPU* cpu, const BInstruction *b_instruction){
	if ((int32_t) cpu->regfile_[b_instruction->rs1] >= (int32_t) cpu->regfile_[b_instruction->rs2]) {
		cpu->pc_ = cpu->pc_ + (int32_t) b_instruction->imm;
	} else {
		cpu->pc_ += 4;
	}
}

/**
 * unsigned Comps
 */
void bltu(CPU* cpu, const BInstruction *b_instruction){
	if (cpu->regfile_[b_instruction->rs1] < cpu->regfile_[b_instruction->rs2]) {
		cpu->pc_ = cpu->pc_ + (int32_t) b_instruction->imm;
	} else {
		cpu->pc_ += 4;
	}
}

void bgeu(CPU* cpu, const BInstruction *b_instruction){
	if (cpu->regfile_[b_instruction->rs1] >= cpu->regfile_[b_instruction->rs2]) {
		cpu->pc_ = cpu->pc_ + (int32_t) b_instruction->imm;
	} else {
		cpu->pc_ += 4;
	}
}


enum funct3_b {
BEQ = 0x0,
BNE = 0x1,
BLT = 0x4,
BGE = 0x5,
BLTU = 0x6,
BGEU = 0x7,
};

void execute_b_instruction(CPU* cpu, const BInstruction *b_instr) {
	switch (b_instr->funct3) {
			case BEQ:
				beq(cpu, b_instr);
				break;
			case BNE:
				bne(cpu, b_instr);
				break;
			case BLT:
				blt(cpu, b_instr);
				break;
			case BGE:
				bge(cpu, b_instr);
				break;
			case BLTU:
				bltu(cpu, b_instr);
				break;
			case BGEU:
				bgeu(cpu, b_instr);
				break;
		}

}
/**
 * U Instructions
 */
uint32_t decode_u_imm(const uint32_t *instruction) {
	uint32_t imm = 0;
	uint32_t imm32_12 = (*instruction >> 12) & 0xFFFFF;
	imm = imm32_12 << 12;
	return imm;
}

UInstruction decode_u_instruction(const uint32_t *instruction) {
	UInstruction u_instr = {
		.rd = decode_register(instruction, RD),
		.imm = decode_u_imm(instruction)
	};
	return u_instr;
}

/**
 * J Instructions
 */
uint32_t decode_j_imm(const uint32_t *instruction) {
	uint32_t imm = 0;
	uint8_t extend = (*instruction >> 31) & 1;
	uint8_t imm20 = extend;
	uint16_t imm19_12 = (*instruction >> 12) & 0xFF;
	uint8_t imm11 = (*instruction >> 20) & 1;
	uint16_t imm10_1 = (*instruction >> 21) & 0x3FF;

	imm = (extend * 0xFFE00000)
		+ (imm20 << 20)
		+ (imm19_12 << 12)
		+ (imm11 << 11)
		+ (imm10_1 << 1);
	return imm;
}

JInstruction decode_j_instruction(const uint32_t *instruction) {
	JInstruction j_instr = {
		.rd = decode_register(instruction, RD),
		.imm = decode_j_imm(instruction)
	};
	return j_instr;
}

/**
 * Instruction fetch Instruction decode, Execute, Memory access, Write back
 */
void CPU_execute(CPU* cpu) {

	uint32_t instruction = *(uint32_t*)(cpu->instr_mem_ + (cpu->pc_  & 0xFFFFF));
	enum opcode_decode opcode = decode_opcode(&instruction);
	// TODO


}
int main() {
	uint32_t instruction = 0xFFFFFF43;
	enum opcode_decode opcode;

	opcode = decode_opcode(instruction);
	printf("%#x", opcode);
}
/*int main(int argc, char* argv[]) {
	printf("C Praktikum\nHU Risc-V  Emulator 2022\n");
	CPU* cpu_inst;

	cpu_inst = CPU_init(argv[1], argv[2]);
    for(uint32_t i = 0; i <1000000; i++) { // run 70000 cycles
    	CPU_execute(cpu_inst);
    }

	printf("\n-----------------------RISC-V program terminate------------------------\nRegfile values:\n");

	//output Regfile
	for(uint32_t i = 0; i <= 31; i++) {
    	printf("%d: %X\n",i,cpu_inst->regfile_[i]);
    }
    fflush(stdout);

	return 0;
	}*/
