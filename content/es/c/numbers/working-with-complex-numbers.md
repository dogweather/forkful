---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:39.219358-07:00
description: "C\xF3mo hacerlo: En C, los n\xFAmeros complejos son soportados por la\
  \ Biblioteca Est\xE1ndar, espec\xEDficamente por `<complex.h>`. Para utilizarlos,\
  \ declara\u2026"
lastmod: '2024-03-13T22:44:59.538566-06:00'
model: gpt-4-0125-preview
summary: "En C, los n\xFAmeros complejos son soportados por la Biblioteca Est\xE1\
  ndar, espec\xEDficamente por `<complex.h>`."
title: "Trabajando con n\xFAmeros complejos"
weight: 14
---

## Cómo hacerlo:
En C, los números complejos son soportados por la Biblioteca Estándar, específicamente por `<complex.h>`. Para utilizarlos, declara variables con el tipo `double complex` (o `float complex` para precisión simple). Aquí se muestra cómo realizar operaciones básicas:

```c
#include <stdio.h>
#include <complex.h>

int main() {
    double complex z1 = 1.0 + 2.0*I; // Declara un número complejo 1+2i
    double complex z2 = 1.0 - 2.0*I; // Declara otro número complejo 1-2i
    
    // Adición
    double complex suma = z1 + z2;
    printf("Suma: %.2f + %.2fi\n", creal(suma), cimag(suma)); // Salida: Suma: 2.00 + 0.00i

    // Multiplicación
    double complex producto = z1 * z2;
    printf("Producto: %.2f + %.2fi\n", creal(producto), cimag(producto)); // Salida: Producto: 5.00 + 0.00i

    // Conjugado Complejo
    double complex conjugado = conj(z1);
    printf("Conjugado de z1: %.2f + %.2fi\n", creal(conjugado), cimag(conjugado)); // Salida: Conjugado de z1: 1.00 - 2.00i
    
    // Magnitud
    double magnitud = cabs(z1);
    printf("Magnitud de z1: %.2f\n", magnitud); // Salida: Magnitud de z1: 2.24

    // Fase
    double fase = carg(z1);
    printf("Fase de z1: %.2f\n", fase); // Salida en radianes
    
    return 0;
}
```
Nota que `I` es una constante que representa la unidad imaginaria en `<complex.h>`. Funciones como `creal()` y `cimag()` extraen las partes real e imaginaria, respectivamente, mientras que `conj()` calcula el conjugado complejo. Para la magnitud y la fase (argumento) de los números complejos, se usan `cabs()` y `carg()`.

## Análisis Profundo
El soporte para números complejos en C es relativamente reciente, habiendo sido estandarizado en C99. Antes de esto, la aritmética de números complejos en C era engorrosa, a menudo requiriendo estructuras de datos y funciones personalizadas. La inclusión de `<complex.h>` y los tipos de datos complejos proporcionaron un impulso significativo a las capacidades del lenguaje para aplicaciones científicas e ingenieriles. Sin embargo, vale la pena señalar que algunos idiomas, como Python, ofrecen un soporte más intuitivo para números complejos a través de tipos de datos incorporados y un conjunto más rico de funciones de biblioteca. A pesar de esto, el rendimiento y el control ofrecidos por C lo convierten en una opción preferida para tareas de computación de alto rendimiento, incluso si eso significa lidiar con una sintaxis ligeramente más verbosa para la aritmética compleja.
