#!/bin/bash

for i in 8 12 16 20 24 28 32
do
	sed -i "s/(define XLEN .*$/(define XLEN $i)/g" riscv-oisc-interpreter.rkt
	racket riscv-oisc-verifications.rkt
done
