/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bw" } */

typedef char v64qi __attribute__ ((vector_size (64)));
typedef short v32hi __attribute__ ((vector_size (64)));
typedef int v16si __attribute__ ((vector_size (64)));
typedef long long v8di __attribute__((vector_size (64)));

v64qi foo_v64qi (char a, v64qi b)
{
    return (__extension__ (v64qi) {~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a,
                                   ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a,
                                   ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a,
                                   ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a,
				   ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a,
				   ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a,
				   ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a,
				   ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a}) & b;
}

v32hi foo_v32hi (short a, v32hi b)
{
    return (__extension__ (v32hi) {~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a,
                                   ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a,
                                   ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a,
				   ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a}) & b;
}

v16si foo_v16si (int a, v16si b)
{
    return (__extension__ (v16si) {~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a,
				   ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a}) & b;
}

v8di foo_v8di (long long a, v8di b)
{
    return (__extension__ (v8di) {~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a}) & b;
}

/* { dg-final { scan-assembler-times "vpandn" 4 } } */
