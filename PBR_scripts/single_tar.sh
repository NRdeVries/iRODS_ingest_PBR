#! /bin/bash
#$ -S /bin/bash
#$ -pe sharedmem 1
#$ -cwd

tar -cf "$@"
