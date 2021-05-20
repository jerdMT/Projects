#ifndef ROTOR_H
#define ROTOR_H

extern double theta[90];
extern double active[90];

double x_pos(int);
double y_pos(int);

int neighbour(int, int);

void initialize(int, char*[]);

void rotor_random(void);

void rotor_align(void);

void rotor_vortex(void);

void update(void);

double Magnetization(void);

double energy(void);

void toggle_lattice(void);

void toggle_sign(void);

void exit_gracefully(void);

#endif // ROTOR_H

