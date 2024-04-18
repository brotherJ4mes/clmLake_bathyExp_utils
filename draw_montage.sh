#!/bin/bash

lks=$(ls ?????.png | cut -c 1-3 | uniq)
for lk in $lks; do 
	echo $lk; 
	montage -geometry 600x250 -tile 3  $lk??.png ../montage/$lk.png; 
done

