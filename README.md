# A Formally Verified Reduction of the RV32I ISA

This is the second part of my Master Thesis at the Institute for Complex Systems @JKU Linz ([ICS](https://www.ics.jku.at/), [GitHub](https://github.com/ics-jku)).

The goal is to formally verify the correctness of the [macros](https://github.com/SonjaGurtner/riscv-oisc-macros) which were written in the first part.
When executing a program with standard RISC-V instructions and one with the new instructions, the architectural state of the resgisters and part of the stack
should be equal. Therefore this Interpreter written in Rosette can take RISC-V and replaced instructions, execute both and compare the result.
Furthermore, the equivalence of the RISC-V Instructions and the new instructions is verified by a SMT Solver, for bit-widths ranging from 8 to 32. This means that no counterexample was found, in which specific register values or a combination of input registers would result in a different architectural state. The valid input is of course restricted, as not every register or value is valid.

With benchmark-verifications.bash the verifier can be called with varying bit length (XLEN) and the verification times per instruction per bit length are stored in respective .dat files in the benchmarks folder.
