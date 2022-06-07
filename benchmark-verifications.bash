#!/bin/bash

FILE=riscv-oisc-verifications.rkt
INTERPRETER=riscv-oisc-interpreter.rkt
BENCHMARKDIR=/home/sonja/GitHub/riscv-oisc-verifier/benchmarks

for solver in  cvc4-kissat cvc4-cadical cvc4-cryptominisat \
			   boolector-cadical boolector-lingeling boolector-cryptominisat \
			   z3 \
			   cvc5-cadical cvc5-kissat cvc5-cryptominisat \
			   bitwuzla-lingeling bitwuzla-cadical bitwuzla-kissat bitwuzla-cryptominisat
do
    echo "******************************************************************************************"
    echo "                            $solver                "
    echo "******************************************************************************************"

    mkdir -p ${BENCHMARKDIR}/${solver}

    sed -i "s|(current-solver (.*))$|(current-solver (${solver}))|g" ${FILE}
    sed -i "s|/benchmarks/.*/~a.dat|/benchmarks/${solver}/~a.dat|g" ${FILE}


    # CVC5
    if [ ${solver} == "cvc5-cadical" ];
    then
        sed -i "s|(current-solver (cvc5.*))$|(current-solver (cvc4 #:logic \"QF_BV\" #:path \"/home/sonja/GitHub/cvc5/build/bin/cvc5\" #:options (hash ':bitblast 'eager ':bv-sat-solver 'cadical)))|g" ${FILE}
    fi;

    if [ ${solver} == "cvc5-kissat" ];
    then
        sed -i "s|(current-solver (cvc5.*))$|(current-solver (cvc4 #:logic \"QF_BV\" #:path \"/home/sonja/GitHub/cvc5/build/bin/cvc5\" #:options (hash ':bitblast 'eager ':bv-sat-solver 'kissat)))|g" ${FILE}
    fi;

    if [ ${solver} == "cvc5-cryptominisat" ];
    then
        sed -i "s|(current-solver (cvc5.*))$|(current-solver (cvc4 #:logic \"QF_BV\" #:path \"/home/sonja/GitHub/cvc5/build/bin/cvc5\" #:options (hash ':bitblast 'eager ':bv-sat-solver 'cryptominisat)))|g" ${FILE}
    fi;

    #CVC4
    if [ ${solver} == "cvc4-cadical" ];
    then
        sed -i "s|(current-solver (cvc4.*))$|(current-solver (cvc4 #:logic \"QF_BV\" #:path \"/home/sonja/GitHub/cvc4/build/bin/cvc4\" #:options (hash ':bitblast 'eager ':bv-sat-solver 'cadical)))|g" ${FILE}
    fi;

    if [ ${solver} == "cvc4-cryptominisat" ];
    then
        sed -i "s|(current-solver (cvc4.*))$|(current-solver (cvc4 #:logic \"QF_BV\" #:path \"/home/sonja/GitHub/cvc4/build/bin/cvc4\" #:options (hash ':bitblast 'eager ':bv-sat-solver 'cryptominisat)))|g" ${FILE}
    fi;

    if [ ${solver} == "cvc4-kissat" ];
    then
        sed -i "s|(current-solver (cvc4.*))$|(current-solver (cvc4 #:logic \"QF_BV\" #:path \"/home/sonja/GitHub/cvc4/build/bin/cvc4\" #:options (hash ':bitblast 'eager ':bv-sat-solver 'kissat)))|g" ${FILE}
    fi;

    if [ ${solver} == "cvc4-cryptominisat" ];
    then
        sed -i "s|(current-solver (cvc4.*))$|(current-solver (cvc4 #:logic \"QF_BV\" #:path \"/home/sonja/GitHub/cvc4/build/bin/cvc4\" #:options (hash ':bitblast 'eager ':bv-sat-solver 'cryptominisat)))|g" ${FILE}
    fi;

    # Bitwuzla
    if [ ${solver} == "bitwuzla-lingeling" ];
    then
        sed -i "s|(current-solver (cvc5.*))$|(current-solver (boolector #:path \"/home/sonja/GitHub/bitwuzla/build/bin/bitwuzla\" #:options (hash ':sat-engine '0)))|g" ${FILE}
    fi;

    if [ ${solver} == "bitwuzla-cadical" ];
    then
        sed -i "s|(current-solver (bitwuzla.*))$|(current-solver (boolector #:path \"/home/sonja/GitHub/bitwuzla/build/bin/bitwuzla\" #:options (hash ':sat-engine '1)))|g" ${FILE}
    fi;

	if [ ${solver} == "bitwuzla-kissat" ];
    then
        sed -i "s|(current-solver (.*))$|(current-solver (boolector #:path \"/home/sonja/GitHub/bitwuzla/build/bin/bitwuzla\" #:options (hash ':sat-engine '2)))|g" ${FILE}
    fi;

    if [ ${solver} == "bitwuzla-cryptominisat" ];
    then
        sed -i "s|(current-solver (.*))$|(current-solver (boolector #:path \"/home/sonja/GitHub/bitwuzla/build/bin/bitwuzla\" #:options (hash ':sat-engine '5)))|g" ${FILE}
    fi;

    #boolector
    if [ ${solver} == "boolector-cadical" ];
    then
        sed -i "s|(current-solver (.*))$|(current-solver (boolector #:path \"/home/sonja/GitHub/boolector-3.2.2/build/bin/boolector\" #:options (hash ':sat-engine '1)))|g" ${FILE}
    fi;

    if [ ${solver} == "boolector-lingeling" ];
    then
        sed -i "s|(current-solver (.*))$|(current-solver (boolector #:path \"/home/sonja/GitHub/boolector-3.2.2/build/bin/boolector\" #:options (hash ':sat-engine '0)))|g" ${FILE}
    fi;

    if [ ${solver} == "boolector-cryptominisat" ];
    then
        sed -i "s|(current-solver (.*))$|(current-solver (boolector #:path \"/home/sonja/GitHub/boolector-3.2.2/build/bin/boolector\" #:options (hash ':sat-engine '4)))|g" ${FILE}
    fi;

    for i in 8 12 16 20 24 28 32
    do
	sed -i "s/(define XLEN .*)$/(define XLEN $i)/g" ${INTERPRETER}
	racket ${FILE}
    done
done

exit