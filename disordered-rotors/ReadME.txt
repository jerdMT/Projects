ReadMe
Last Updated: 02/6/20



I've produced a few sample plots from the data rotor.cpp has produced.
At present, I am seeing some irregularities/unexpected behavior in the figures that need to be addressed. Some seeds from the Mersenne twister PRNG are producing different initial magnetization values and this is an issue. (As I should expect the same value independent of seed? see data.txt)

Also, I have not written code to produce a general array of p-values. At present, the program operates off of code that works for a lattice arrangement of 90-site. I plan on implementing changes soon to accommodate for generalization. 


