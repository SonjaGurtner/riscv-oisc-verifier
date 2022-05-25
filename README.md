# RISC-V OISC Rosette

This is the second part of my Master Thesis at [ICS](https://www.ics.jku.at/)@JKU.
The goal is to formally verify the correctness of the [macros](https://github.com/SonjaGurtner/riscv-oisc-macros) which were written in the first part.
When executing a program with standard RISC-V instructions and one with the new instructions, the architectural state of the resgisters and the stack
should be equal. Therefore this Interpretor written in Rosette can take RISC-V and replaced instructions, execute both and compare the result.
Furthermore, the equivalence of the RISC-V Instructions and the new instructions is verified by a SMT Solver. This means that no counterexample can be found, in which specific register values or combination of input registers would lead to a different result. The valid input is of course restricted, as not every register or value is valid.
