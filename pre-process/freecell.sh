#!/bin/sh

for num_poss_pre in 0 1 2 3 4 5
do
    for num_poss_add in 0 1 2 3 4 5
    do
	for num_poss_del in 0 1 2 3 4 5
	do
	    for dom_count in 0
	    do
		for prob_count in 1 2 3 4 5 6 7 8 9 10
		do
		    n=$((num_poss_pre + num_poss_add + num_poss_del))
		    if [ "$n" -gt 0 ]
		    then
			domain=freecell.pddl.${num_poss_pre}p_${num_poss_add}a_${num_poss_del}d.${dom_count}
			problem=pfile$prob_count
			input=${domain}_${problem}.out
			output=freecell@default.csv
			echo "Processing" ${input} " --> output to " ${output} 
			./read-default ./default-freecell-raw/ ${input} ./ ${output} ${domain} ${problem}
		    fi
		done
	    done
	done
    done
done