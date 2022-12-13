
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

enum opcode_decode {
  R = 0x33,
  I = 0x13,
  S = 0x23,
  L = 0x03,
  B = 0x63,
  JALR = 0x67,
  JAL = 0x6F,
  AUIPC = 0x17,
  LUI = 0x37
};

typedef struct {
  size_t data_mem_size_;
  uint32_t regfile_[32];
  uint32_t pc_;
  uint8_t *instr_mem_;
  uint8_t *data_mem_;
} CPU;

void CPU_open_instruction_mem(CPU *cpu, const char *filename);
void CPU_load_data_mem(CPU *cpu, const char *filename);

CPU *CPU_init(const char *path_to_inst_mem, const char *path_to_data_mem) {
  CPU *cpu = (CPU *)malloc(sizeof(CPU));
  cpu->data_mem_size_ = 0x400000;
  cpu->pc_ = 0x0;
  CPU_open_instruction_mem(cpu, path_to_inst_mem);
  CPU_load_data_mem(cpu, path_to_data_mem);
  return cpu;
}

void CPU_open_instruction_mem(CPU *cpu, const char *filename) {
  uint32_t instr_mem_size;
  FILE *input_file = fopen(filename, "r");
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
  printf("size of instruction memory: %d Byte\n\n", sb.st_size);
  instr_mem_size = sb.st_size;
  cpu->instr_mem_ = malloc(instr_mem_size);
  fread(cpu->instr_mem_, sb.st_size, 1, input_file);
  fclose(input_file);
  return;
}

void CPU_load_data_mem(CPU *cpu, const char *filename) {
  FILE *input_file = fopen(filename, "r");
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
  printf("read data for data memory: %d Byte\n\n", sb.st_size);

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
  // read the first 7 Bits of the instruction
  opcode = (*instruction & 0x7F);
  return opcode;
}

/**
 * Function for extracting register operand values from instructions
 */
enum register_position { RS1, RS2, RD };
uint8_t decode_register(const uint32_t *instruction,
                        const enum register_position reg_pos) {
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
  RInstruction r_instr = {.rs1 = decode_register(instruction, RS1),
                          .rs2 = decode_register(instruction, RS2),
                          .rd = decode_register(instruction, RD),
                          .funct3 = decode_funct3(instruction),
                          .funct7 = decode_funct7(instruction)};
  return r_instr;
}

void add(CPU *cpu, const RInstruction *r_instruction) {
  cpu->regfile_[r_instruction->rd] =
      cpu->regfile_[r_instruction->rs1] + cpu->regfile_[r_instruction->rs2];
  cpu->pc_ += 4;
}

void sub(CPU *cpu, const RInstruction *r_instruction) {
  cpu->regfile_[r_instruction->rd] =
      cpu->regfile_[r_instruction->rs1] - cpu->regfile_[r_instruction->rs2];
  cpu->pc_ += 4;
}

void and_(CPU *cpu, const RInstruction *r_instruction) {
  cpu->regfile_[r_instruction->rd] =
      cpu->regfile_[r_instruction->rs1] & cpu->regfile_[r_instruction->rs2];
  cpu->pc_ += 4;
}

void or_(CPU *cpu, const RInstruction *r_instruction) {
  cpu->regfile_[r_instruction->rd] =
      cpu->regfile_[r_instruction->rs1] | cpu->regfile_[r_instruction->rs2];
  cpu->pc_ += 4;
}

void xor_(CPU *cpu, const RInstruction *r_instruction) {
  cpu->regfile_[r_instruction->rd] =
      cpu->regfile_[r_instruction->rs1] ^ cpu->regfile_[r_instruction->rs2];
  cpu->pc_ += 4;
}

void srl(CPU *cpu, const RInstruction *r_instruction) {
  cpu->regfile_[r_instruction->rd] =
      cpu->regfile_[r_instruction->rs1] >> cpu->regfile_[r_instruction->rs2];
  cpu->pc_ += 4;
}
// TODO change to arithmetic shift (keep sign)
void sra(CPU *cpu, const RInstruction *r_instruction) {
  cpu->regfile_[r_instruction->rd] =
      ((int32_t)cpu->regfile_[r_instruction->rs1]) >>
      cpu->regfile_[r_instruction->rs2];
  cpu->pc_ += 4;
}

void sll(CPU *cpu, const RInstruction *r_instruction) {
  cpu->regfile_[r_instruction->rd] = cpu->regfile_[r_instruction->rs1]
                                     << cpu->regfile_[r_instruction->rs2];
  cpu->pc_ += 4;
}

void slt(CPU *cpu, const RInstruction *r_instruction) {
  cpu->regfile_[r_instruction->rd] =
      (int32_t)cpu->regfile_[r_instruction->rs1] <
      (int32_t)cpu->regfile_[r_instruction->rs2];
  cpu->pc_ += 4;
}

void sltu(CPU *cpu, const RInstruction *r_instruction) {
  cpu->regfile_[r_instruction->rd] =
      cpu->regfile_[r_instruction->rs1] < cpu->regfile_[r_instruction->rs2];
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

void execute_r_instruction(CPU *cpu, RInstruction *r_instruction) {
  switch (r_instruction->funct3) {
  case ADD_SUB:
    if (r_instruction->funct7 == 0) {
      add(cpu, r_instruction);
    } else {
      sub(cpu, r_instruction);
    }
    break;
  case AND:
    and_(cpu, r_instruction);
    break;
  case OR:
    or_(cpu, r_instruction);
    break;
  case XOR:
    xor_(cpu, r_instruction);
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
enum funct3_load {
  LB = 0x0,
  LH = 0x1,
  LW = 0x2,
  LBU = 0x4,
  LHU = 0x5,
};
uint32_t decode_i_imm(const uint32_t *instruction) {
  uint32_t imm = 0;
  uint32_t extend = (*instruction >> 31) & 1;
  uint32_t imm11_0 = (*instruction >> 20) & 0xFFF;
  imm = (extend * 0xFFFFF000) + imm11_0;

  return imm;
}

IInstruction decode_i_instruction(const uint32_t *instruction) {
  IInstruction i_instr = {.rs1 = decode_register(instruction, RS1),
                          .rd = decode_register(instruction, RD),
                          .funct3 = decode_funct3(instruction),
                          .imm = decode_i_imm(instruction)};
  return i_instr;
}

void lb(CPU *cpu, const IInstruction *i_instruction) {
  uint8_t data_to_load = 0;
  uint8_t sign_extend;
  data_to_load =
      cpu->data_mem_[cpu->regfile_[i_instruction->rs1] + i_instruction->imm];
  sign_extend = (data_to_load >> 7) & 1;
  cpu->regfile_[i_instruction->rd] = (0xFFFFFF00 * sign_extend) + data_to_load;
  cpu->pc_ += 4;
}

void lh(CPU *cpu, const IInstruction *i_instruction) {
  uint16_t data_to_load = 0;
  uint8_t sign_extend;
  data_to_load = *(uint16_t *)(cpu->regfile_[i_instruction->rs1] +
                               i_instruction->imm + cpu->data_mem_);
  sign_extend = (data_to_load >> 15) & 1;
  cpu->regfile_[i_instruction->rd] = (0xFFFF0000 * sign_extend) + data_to_load;
  cpu->pc_ += 4;
}

void lw(CPU *cpu, const IInstruction *i_instruction) {
  cpu->regfile_[i_instruction->rd] =
      *(uint32_t *)(cpu->regfile_[i_instruction->rs1] + i_instruction->imm +
                    cpu->data_mem_);
  cpu->pc_ += 4;
}

void lbu(CPU *cpu, const IInstruction *i_instruction) {
  cpu->regfile_[i_instruction->rd] =
      cpu->data_mem_[cpu->regfile_[i_instruction->rs1] + i_instruction->imm] &
      0xFF;
  cpu->pc_ += 4;
}

void lhu(CPU *cpu, const IInstruction *i_instruction) {
  cpu->regfile_[i_instruction->rd] =
      *(uint16_t *)(cpu->regfile_[i_instruction->rs1] + i_instruction->imm +
                    cpu->data_mem_);
  cpu->pc_ += 4;
}

void addi(CPU *cpu, const IInstruction *i_instruction) {
  cpu->regfile_[i_instruction->rd] =
      cpu->regfile_[i_instruction->rs1] + i_instruction->imm;
  cpu->pc_ += 4;
}

void slti(CPU *cpu, const IInstruction *i_instruction) {
  cpu->regfile_[i_instruction->rd] =
      (int32_t)cpu->regfile_[i_instruction->rs1] < (int32_t)i_instruction->imm;
  cpu->pc_ += 4;
}

void sltiu(CPU *cpu, const IInstruction *i_instruction) {
  if (i_instruction->imm == 1 && i_instruction->rs1 == 0) {
    cpu->regfile_[i_instruction->rd] = 0;
  } else {
    cpu->regfile_[i_instruction->rd] =
        cpu->regfile_[i_instruction->rs1] < i_instruction->imm;
  }
  cpu->pc_ += 4;
}

void xori(CPU *cpu, const IInstruction *i_instruction) {
  cpu->regfile_[i_instruction->rd] =
      cpu->regfile_[i_instruction->rs1] ^ i_instruction->imm;
  cpu->pc_ += 4;
}

void ori(CPU *cpu, const IInstruction *i_instruction) {
  cpu->regfile_[i_instruction->rd] =
      cpu->regfile_[i_instruction->rs1] | i_instruction->imm;
  cpu->pc_ += 4;
}

void andi(CPU *cpu, const IInstruction *i_instruction) {
  cpu->regfile_[i_instruction->rd] =
      cpu->regfile_[i_instruction->rs1] & i_instruction->imm;
  cpu->pc_ += 4;
}

void jalr(CPU *cpu, const IInstruction *i_instruction) {
  cpu->regfile_[i_instruction->rd] = cpu->pc_ + 4;
  cpu->pc_ =
      (cpu->regfile_[i_instruction->rs1]) + ((int32_t)i_instruction->imm) &
      0xFFFFFFFE;
}
/**
 * Instructions that take R-like instructions */
void slli(CPU *cpu, const RInstruction *r_instruction) {
  cpu->regfile_[r_instruction->rd] = cpu->regfile_[r_instruction->rs1]
                                     << r_instruction->rs2;
  cpu->pc_ += 4;
}

void srli(CPU *cpu, const RInstruction *r_instruction) {
  cpu->regfile_[r_instruction->rd] =
      cpu->regfile_[r_instruction->rs1] >> r_instruction->rs2;
  cpu->pc_ += 4;
}
// fix arithmetic shift
void srai(CPU *cpu, const RInstruction *r_instruction) {
  cpu->regfile_[r_instruction->rd] =
      ((int32_t)cpu->regfile_[r_instruction->rs1]) >> r_instruction->rs2;
  cpu->pc_ += 4;
}

enum funct3_i {
  ADDI = 0x0,
  SLTI = 0x2,
  SLTIU = 0x3,
  XORI = 0x4,
  ORI = 0x6,
  ANDI = 0x7,
};
void execute_i_instruction(CPU *cpu, const IInstruction *i_instruction) {
  switch (i_instruction->funct3) {
  case ADDI:
    addi(cpu, i_instruction);
    break;
  case SLTI:
    slti(cpu, i_instruction);
    break;
  case SLTIU:
    sltiu(cpu, i_instruction);
    break;
  case XORI:
    xori(cpu, i_instruction);
    break;
  case ORI:
    ori(cpu, i_instruction);
    break;
  case ANDI:
    andi(cpu, i_instruction);
    break;
  }
}

void execute_load_instruction(CPU *cpu, const IInstruction *i_instruction) {
  switch (i_instruction->funct3) {
  case LB:
    lb(cpu, i_instruction);
    break;
  case LH:
    lh(cpu, i_instruction);
    break;
  case LW:
    lw(cpu, i_instruction);
    break;
  case LBU:
    lbu(cpu, i_instruction);
    break;
  case LHU:
    lhu(cpu, i_instruction);
    break;
  }
}
/**
 * S Instructions
 */
uint32_t decode_s_imm(const uint32_t *instruction) {
  uint32_t imm = 0;
  uint32_t extend = (*instruction >> 31) & 1;
  uint32_t imm11_5 = (*instruction >> 25) & 0x7F;
  uint32_t imm4_0 = (*instruction >> 7) & 0x1F;
  imm = (0xFFFFF000 * extend) + (imm11_5 << 5) + imm4_0;
  return imm;
}

SInstruction decode_s_instruction(const uint32_t *instruction) {
  SInstruction s_instr = {.rs1 = decode_register(instruction, RS1),
                          .rs2 = decode_register(instruction, RS2),
                          .funct3 = decode_funct3(instruction),
                          .imm = decode_s_imm(instruction)};
  return s_instr;
}

void sb(CPU *cpu, const SInstruction *s_instruction) {
  if ((uint32_t)(cpu->regfile_[s_instruction->rs1] +
                 (int32_t)s_instruction->imm) == 0x5000) {
    putchar((uint8_t)cpu->regfile_[s_instruction->rs2]);
  }
  cpu->data_mem_[cpu->regfile_[s_instruction->rs1] +
                 (int32_t)s_instruction->imm] =
      (uint8_t)cpu->regfile_[s_instruction->rs2];
  cpu->pc_ += 4;
}

void sh(CPU *cpu, const SInstruction *s_instruction) {
  *(uint16_t *)(cpu->data_mem_ +
                (cpu->regfile_[s_instruction->rs1] + s_instruction->imm)) =
      (uint16_t)cpu->regfile_[s_instruction->rs2];
  cpu->pc_ += 4;
}

void sw(CPU *cpu, const SInstruction *s_instruction) {
  *(uint32_t *)(cpu->data_mem_ +
                (cpu->regfile_[s_instruction->rs1] + s_instruction->imm)) =
      (uint32_t)cpu->regfile_[s_instruction->rs2];
  cpu->pc_ += 4;
}

enum funct3_s { SB = 0x0, SH = 0x1, SW = 0x2 };

void execute_s_instruction(CPU *cpu, const SInstruction *s_instruction) {
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
  uint32_t extend = (*instruction >> 31) & 1;
  uint32_t imm12 = extend;
  uint32_t imm11 = (*instruction >> 7) & 1;
  uint32_t imm10_5 = (*instruction >> 25) & 0x3F;
  uint32_t imm4_1 = (*instruction >> 8) & 0xF;

  imm = (extend * 0xFFFFE000) + (imm12 << 12) + (imm11 << 11) + (imm10_5 << 5) +
        (imm4_1 << 1);
  return imm;
}

BInstruction decode_b_instruction(const uint32_t *instruction) {
  BInstruction b_instr = {.rs1 = decode_register(instruction, RS1),
                          .rs2 = decode_register(instruction, RS2),
                          .funct3 = decode_funct3(instruction),
                          .imm = decode_b_imm(instruction)};
  return b_instr;
}

void beq(CPU *cpu, const BInstruction *b_instruction) {
  if (cpu->regfile_[b_instruction->rs1] == cpu->regfile_[b_instruction->rs2]) {
    cpu->pc_ = cpu->pc_ + (int32_t)b_instruction->imm;
  } else {
    cpu->pc_ += 4;
  }
}
void bne(CPU *cpu, const BInstruction *b_instruction) {
  if (cpu->regfile_[b_instruction->rs1] != cpu->regfile_[b_instruction->rs2]) {
    cpu->pc_ = cpu->pc_ + (int32_t)b_instruction->imm;
  } else {
    cpu->pc_ += 4;
  }
}

/**
 * Signed Comps
 */
void blt(CPU *cpu, const BInstruction *b_instruction) {
  if ((int32_t)cpu->regfile_[b_instruction->rs1] <
      (int32_t)cpu->regfile_[b_instruction->rs2]) {
    cpu->pc_ = cpu->pc_ + (int32_t)b_instruction->imm;
  } else {
    cpu->pc_ += 4;
  }
}

void bge(CPU *cpu, const BInstruction *b_instruction) {
  if ((int32_t)cpu->regfile_[b_instruction->rs1] >=
      (int32_t)cpu->regfile_[b_instruction->rs2]) {
    cpu->pc_ = cpu->pc_ + (int32_t)b_instruction->imm;
  } else {
    cpu->pc_ += 4;
  }
}

/**
 * unsigned Comps
 */
void bltu(CPU *cpu, const BInstruction *b_instruction) {
  if (cpu->regfile_[b_instruction->rs1] < cpu->regfile_[b_instruction->rs2]) {
    cpu->pc_ = cpu->pc_ + (int32_t)b_instruction->imm;
  } else {
    cpu->pc_ += 4;
  }
}

void bgeu(CPU *cpu, const BInstruction *b_instruction) {
  if (cpu->regfile_[b_instruction->rs1] >= cpu->regfile_[b_instruction->rs2]) {
    cpu->pc_ = cpu->pc_ + (int32_t)b_instruction->imm;
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

void execute_b_instruction(CPU *cpu, const BInstruction *b_instr) {
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
  uint32_t imm31_12 = (*instruction) & 0xFFFFF000;
  imm = imm31_12;
  return imm;
}

UInstruction decode_u_instruction(const uint32_t *instruction) {
  UInstruction u_instr = {.rd = decode_register(instruction, RD),
                          .imm = decode_u_imm(instruction)};
  return u_instr;
}

void lui(CPU *cpu, const UInstruction *u_instruction) {
  cpu->regfile_[u_instruction->rd] = u_instruction->imm;
  cpu->pc_ += 4;
}

void auipc(CPU *cpu, const UInstruction *u_instruction) {
  cpu->regfile_[u_instruction->rd] = cpu->pc_ + u_instruction->imm;
  cpu->pc_ += 4;
}

/**
 * J Instructions
 */
uint32_t decode_j_imm(const uint32_t *instruction) {
  uint32_t imm = 0;
  uint32_t extend = (*instruction >> 31) & 1;
  uint32_t imm20 = extend;
  uint32_t imm19_12 = (*instruction >> 12) & 0xFF;
  uint32_t imm11 = (*instruction >> 20) & 1;
  uint32_t imm10_1 = (*instruction >> 21) & 0x3FF;

  imm = (extend * 0xFFE00000) + (imm20 << 20) + (imm19_12 << 12) +
        (imm11 << 11) + (imm10_1 << 1);
  return imm;
}

JInstruction decode_j_instruction(const uint32_t *instruction) {
  JInstruction j_instr = {.rd = decode_register(instruction, RD),
                          .imm = decode_j_imm(instruction)};
  return j_instr;
}

void jal(CPU *cpu, const JInstruction *j_instruction) {
  cpu->regfile_[j_instruction->rd] = cpu->pc_ + 4;
  cpu->pc_ = cpu->pc_ + (int32_t)j_instruction->imm;
}

/**
 * Instruction fetch Instruction decode, Execute, Memory access, Write back
 */
void CPU_execute(CPU *cpu) {
  cpu->regfile_[0] = 0;
  uint32_t instruction = *(uint32_t *)(cpu->instr_mem_ + (cpu->pc_ & 0xFFFFF));
  enum opcode_decode opcode = decode_opcode(&instruction);
  // TODO
  switch (opcode) {
  case R: {
    RInstruction r_instr = decode_r_instruction(&instruction);
    execute_r_instruction(cpu, &r_instr);
    break;
  }
  case I: {
    uint8_t funct3 = decode_funct3(&instruction);
    uint8_t funct7 = decode_funct7(&instruction);
    // if SSLI
    if (funct3 == 1) {
      RInstruction r_instr = decode_r_instruction(&instruction);
      slli(cpu, &r_instr);
    }
    // or srli/srai
    else if (funct3 == 5) {
      RInstruction r_instr = decode_r_instruction(&instruction);
      if (funct7 == 0) {
        srli(cpu, &r_instr);
      } else {
        srai(cpu, &r_instr);
      }
    }
    // else execute normal I-Instruction
    else {
      IInstruction i_instr = decode_i_instruction(&instruction);
      execute_i_instruction(cpu, &i_instr);
    }
    break;
  }
  case S: {
    SInstruction s_instr = decode_s_instruction(&instruction);
    execute_s_instruction(cpu, &s_instr);
    break;
  }
  case L: {
    IInstruction i_instr = decode_i_instruction(&instruction);
    execute_load_instruction(cpu, &i_instr);
    break;
  }
  case B: {
    BInstruction b_instr = decode_b_instruction(&instruction);
    execute_b_instruction(cpu, &b_instr);
    break;
  }
  case JALR: {
    IInstruction i_instr = decode_i_instruction(&instruction);
    jalr(cpu, &i_instr);
    break;
  }
  case JAL: {
    JInstruction j_instr = decode_j_instruction(&instruction);
    jal(cpu, &j_instr);
    break;
  }
  case AUIPC: {
    UInstruction u_instr = decode_u_instruction(&instruction);
    auipc(cpu, &u_instr);
    break;
  }
  case LUI: {
    UInstruction u_instr = decode_u_instruction(&instruction);
    lui(cpu, &u_instr);
    break;
  }
  }
  cpu->regfile_[0] = 0;
}
/**
 * Unit Tests
 */
#define SUCC_FAIL_RETURN                                                       \
  printf("SUCCESS\n");                                                         \
  return;                                                                      \
  error:                                                                       \
  printf("FAILURE\n");

#define TESTEQ(arg1, arg2)                                                     \
  if (arg1 != arg2) {                                                          \
    goto error;                                                                \
  }

#define TESTNEQ(arg1, arg2)                                                    \
  if (arg1 == arg2) {                                                          \
    goto error;                                                                \
  }

void test_opcode_decode() {
  uint8_t opcode = B;
  uint32_t instruction = opcode;
  printf("test_opcode_decode(): ");
  TESTEQ(decode_opcode(&instruction), opcode);
  SUCC_FAIL_RETURN;
}

void test_decode_register() {
  uint8_t rs1 = 0x2;
  uint8_t rs2 = 0x5;
  uint8_t rd = 0x7;
  uint32_t instruction = 0;
  instruction = (rs2 << 20) + (rs1 << 15) + (rd << 7);

  printf("test_decode_register(): ");
  TESTEQ(decode_register(&instruction, RS1), rs1);
  TESTEQ(decode_register(&instruction, RS2), rs2);
  TESTEQ(decode_register(&instruction, RD), rd);
  SUCC_FAIL_RETURN;
}

void test_decode_funct3() {
  uint8_t funct3 = 0x7;
  uint32_t instruction = 0;
  instruction = funct3 << 12;

  printf("test_decode_funct3(): ");
  TESTEQ(decode_funct3(&instruction), funct3);
  SUCC_FAIL_RETURN;
}

void test_decode_funct7() {
  uint8_t funct7 = 0x7F;
  uint32_t instruction = 0;
  instruction = funct7 << 25;

  printf("test_decode_funct7(): ");
  TESTEQ(decode_funct7(&instruction), funct7);
  SUCC_FAIL_RETURN;
}

void test_decode_r_instruction() {
  printf("test_decode_r_instruction(): ");
  RInstruction ri = {
      .rs1 = 0x1F, .rs2 = 0x1F, .rd = 0x1F, .funct3 = 0x7, .funct7 = 0x7F};
  uint32_t instruction = 0;
  instruction = (ri.rs1 << 15) + (ri.rs2 << 20) + (ri.rd << 7) +
                (ri.funct3 << 12) + (ri.funct7 << 25);
  RInstruction ri_d = decode_r_instruction(&instruction);
  TESTEQ(ri.rs1, ri_d.rs1);
  TESTEQ(ri.rs2, ri_d.rs2);
  TESTEQ(ri.rd, ri_d.rd);
  TESTEQ(ri.funct3, ri_d.funct3);
  TESTEQ(ri.funct7, ri_d.funct7);
  SUCC_FAIL_RETURN;
}

void test_decode_i_instruction() {
  printf("test_decode_i_instruction(): ");
  uint16_t imm = 0x7FF;
  IInstruction ii = {
      .rs1 = 0x1F,
      .rd = 0x1F,
      .funct3 = 0x7,
      .imm = imm,
  };
  uint32_t instruction = 0;
  instruction = (imm << 20) + (ii.rs1 << 15) + (ii.funct3 << 12) + (ii.rd << 7);
  IInstruction ii_d = decode_i_instruction(&instruction);
  TESTEQ(ii.rs1, ii_d.rs1);
  TESTEQ(ii.rd, ii_d.rd);
  TESTEQ(ii.funct3, ii_d.funct3);
  TESTEQ(ii.imm, ii_d.imm);
  // test also for sign extention
  imm = 0xFFF;
  ii.imm = imm;
  instruction = 0;
  instruction = (imm << 20) + (ii.rs1 << 15) + (ii.funct3 << 12) + (ii.rd << 7);
  ii_d = decode_i_instruction(&instruction);
  TESTEQ(0xFFFFFFFF, ii_d.imm);
  SUCC_FAIL_RETURN;
}

void test_decode_s_instruction() {
  printf("test_decode_s_instruction(): ");
  SInstruction si = {
      .rs1 = 0x1F,
      .rs2 = 0x1F,
      .funct3 = 0x7,
      .imm = 0x7FF,
  };
  uint32_t instruction = 0;
  instruction = ((si.imm & 0x1F) << 7) + (si.funct3 << 12) + (si.rs1 << 15) +
                (si.rs2 << 20) + ((si.imm & 0xFE0) << 20);
  SInstruction si_d = decode_s_instruction(&instruction);
  TESTEQ(si.rs1, si_d.rs1);
  TESTEQ(si.rs2, si_d.rs2);
  TESTEQ(si.funct3, si_d.funct3);
  TESTEQ(si.imm, si_d.imm);
  si.imm = 0xFFF;
  instruction = 0;
  instruction = ((si.imm & 0x1F) << 7) + (si.funct3 << 12) + (si.rs1 << 15) +
                ((si.imm & 0xFE0) << 20);
  si_d = decode_s_instruction(&instruction);
  TESTEQ(0xFFFFFFFF, si_d.imm);
  SUCC_FAIL_RETURN;
}

void test_decode_b_instruction() {
  printf("test_decode_b_instruction(): ");
  BInstruction bi = {
      .rs1 = 0x1F,
      .rs2 = 0x1F,
      .funct3 = 0x7,
      .imm = 0xFFE,
  };
  uint32_t instruction = 0;
  instruction = (bi.rs1 << 15) + (bi.rs2 << 20) + (bi.funct3 << 12) +
                ((bi.imm & 0x1000) << 19) + ((bi.imm & 0x7e0) << 20) +
                ((bi.imm & 0x1E) << 7) + ((bi.imm & 0x800) >> 4);
  BInstruction bi_d = decode_b_instruction(&instruction);
  TESTEQ(bi.rs1, bi_d.rs1);
  TESTEQ(bi.rs2, bi_d.rs2);
  TESTEQ(bi.funct3, bi_d.funct3);
  TESTEQ(bi.imm, bi_d.imm);
  bi.imm = 0x1FFE;
  instruction = 0;
  instruction = (bi.rs1 << 15) + (bi.rs2 << 20) + (bi.funct3 << 12) +
                ((bi.imm & 0x1000) << 19) + ((bi.imm & 0x7e0) << 20) +
                ((bi.imm & 0x1E) << 7) + ((bi.imm & 0x800) >> 4);

  bi_d = decode_b_instruction(&instruction);
  TESTEQ(0xFFFFFFFE, bi_d.imm);
  SUCC_FAIL_RETURN;
}

void test_decode_u_instruction() {
  printf("test_decode_u_instruction: ");
  UInstruction ui = {.rd = 0x1F, .imm = 0xFFFFF000};
  uint32_t instruction = 0;
  instruction = (ui.imm) + (ui.rd << 7);
  UInstruction ui_d = decode_u_instruction(&instruction);
  TESTEQ(ui.rd, ui_d.rd);
  TESTEQ(ui.imm, ui_d.imm);
  SUCC_FAIL_RETURN;
}
void test_decode_j_instruction() {
  printf("test_decode_j_instruction: ");
  JInstruction ji = {
      .rd = 0x1F,
      .imm = 0xFFFFE,
  };
  uint32_t instruction = 0;
  instruction = (ji.rd << 7) + (ji.imm & 0xFF000) + ((ji.imm & 0x800) << 9) +
                ((ji.imm & 0x7FE) << 20) + ((ji.imm & 0x100000) << 11);
  JInstruction ji_d = decode_j_instruction(&instruction);
  TESTEQ(ji.rd, ji_d.rd);
  TESTEQ(ji.imm, ji_d.imm);
  ji.imm = 0x1FFFFE;
  instruction = (ji.rd << 7) + (ji.imm & 0xFF000) + ((ji.imm & 0x800) << 9) +
                ((ji.imm & 0x7FE) << 20) + ((ji.imm & 0x100000) << 11);
  ji_d = decode_j_instruction(&instruction);
  TESTEQ(0xFFFFFFFE, ji_d.imm);
  SUCC_FAIL_RETURN;
}

void unit_tests() {
  test_opcode_decode();
  test_decode_register();
  test_decode_funct3();
  test_decode_funct7();

  test_decode_r_instruction();
  test_decode_i_instruction();
  test_decode_s_instruction();
  test_decode_b_instruction();
  test_decode_u_instruction();
  test_decode_j_instruction();
}
// int main() {
//   unit_tests();
//   return 0;
// }

int main(int argc, char *argv[]) {
  printf("C Praktikum\nHU Risc-V  Emulator 2022\n");
  // unit_tests();
  CPU *cpu_inst;

  cpu_inst = CPU_init(argv[1], argv[2]);
  for (uint32_t i = 0; i < 140000; i++) { // run 70000 cycles
    CPU_execute(cpu_inst);
  }

  printf("\n-----------------------RISC-V program "
         "terminate------------------------\nRegfile values:\n");

  // output Regfile
  for (uint32_t i = 0; i <= 31; i++) {
    printf("%d: %X\n", i, cpu_inst->regfile_[i]);
  }
  fflush(stdout);

  return 0;
}
