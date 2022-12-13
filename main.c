
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
	size_t imm;
} IInstruction;

/**
 * Function to extract the opcode from an instruction
 */
static inline enum opcode_decode decode_opcode(const uint32_t instruction) {
	enum opcode_decode opcode;
	// read the first 7 Bytes of the instruction
	opcode = (instruction & 0x7F);
	return opcode;
}

/**
 * Instruction fetch Instruction decode, Execute, Memory access, Write back
 */
void CPU_execute(CPU* cpu) {

	uint32_t instruction = *(uint32_t*)(cpu->instr_mem_ + (cpu->pc_  & 0xFFFFF));
	enum opcode_decode opcode = decode_opcode(instruction);
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
