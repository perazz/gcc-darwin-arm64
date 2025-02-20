/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv_zvfh -mabi=lp64d -Wno-pedantic -Wno-psabi" } */

#include <stdint-gcc.h>

typedef int64_t vnx8di __attribute__((vector_size (64)));
typedef int32_t vnx16si __attribute__((vector_size (64)));
typedef int16_t vnx32hi __attribute__((vector_size (64)));
typedef int8_t vnx64qi __attribute__((vector_size (64)));
typedef _Float16 vnx32hf __attribute__((vector_size (64)));
typedef float vnx16sf __attribute__((vector_size (64)));
typedef double vnx8df __attribute__((vector_size (64)));

#define VEC_EXTRACT(S,V,IDX)			\
  S						\
  __attribute__((noipa))			\
  vec_extract_##V##_##IDX (V v)			\
  {						\
    return v[IDX];				\
  }

#define TEST_ALL3(T)				\
  T (_Float16, vnx32hf, 0)			\
  T (_Float16, vnx32hf, 3)			\
  T (_Float16, vnx32hf, 7)			\
  T (_Float16, vnx32hf, 8)			\
  T (_Float16, vnx32hf, 16)			\
  T (_Float16, vnx32hf, 31)			\
  T (float, vnx16sf, 0)				\
  T (float, vnx16sf, 2)				\
  T (float, vnx16sf, 6)				\
  T (float, vnx16sf, 8)				\
  T (float, vnx16sf, 14)			\
  T (double, vnx8df, 0)				\
  T (double, vnx8df, 2)				\
  T (double, vnx8df, 4)				\
  T (double, vnx8df, 6)				\
  T (int64_t, vnx8di, 0)			\
  T (int64_t, vnx8di, 2)			\
  T (int64_t, vnx8di, 4)			\
  T (int64_t, vnx8di, 6)			\
  T (int32_t, vnx16si, 0)			\
  T (int32_t, vnx16si, 2)			\
  T (int32_t, vnx16si, 6)			\
  T (int32_t, vnx16si, 8)			\
  T (int32_t, vnx16si, 14)			\
  T (int16_t, vnx32hi, 0)			\
  T (int16_t, vnx32hi, 2)			\
  T (int16_t, vnx32hi, 14)			\
  T (int16_t, vnx32hi, 16)			\
  T (int16_t, vnx32hi, 30)			\
  T (int8_t, vnx64qi, 0)			\
  T (int8_t, vnx64qi, 2)			\
  T (int8_t, vnx64qi, 30)			\
  T (int8_t, vnx64qi, 32)			\
  T (int8_t, vnx64qi, 63)			\

TEST_ALL3 (VEC_EXTRACT)

/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e8,\s*m4,\s*ta,\s*ma} 5 } } */
/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e16,\s*m4,\s*ta,\s*ma} 11 } } */
/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e32,\s*m4,\s*ta,\s*ma} 10 } } */
/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e64,\s*m4,\s*ta,\s*ma} 8 } } */

/* { dg-final { scan-assembler-times {\tvslidedown.vi} 25 } } */
/* { dg-final { scan-assembler-times {\tvslidedown.vx} 2 } } */

/* { dg-final { scan-assembler-times {\tvfmv.f.s} 15 } } */
/* { dg-final { scan-assembler-times {\tvmv.x.s} 19 } } */

/* { dg-final { scan-assembler-not {\tsext} } } */
