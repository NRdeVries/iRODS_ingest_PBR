#! /bin/bash
iinit
echo the last PROJID used is:
ils /ArchvPROD/PSG/PBR | grep "C-" | egrep -o "PBR[0-9]+" | sort -r | head -n 1

echo the last EXPID used is:
ils /ArchvPROD/PSG/PBR/ -r | grep "C-" | egrep -o "E[0-9]{6}" | sort -r | uniq | head -n 1
