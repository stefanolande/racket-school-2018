#lang brag
taco-program: [/"\n"]*  (taco-leaf)+  [/"\n"]*
taco-leaf: /"#" (taco | not-a-taco)* /"$"
taco: /"%"
not-a-taco: /"#"/"$"