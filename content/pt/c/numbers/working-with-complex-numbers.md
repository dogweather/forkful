---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:53.740793-07:00
description: "Como Fazer: Em C, os n\xFAmeros complexos s\xE3o suportados pela Biblioteca\
  \ Padr\xE3o, especificamente `<complex.h>`. Para utiliz\xE1-los, declare vari\xE1\
  veis com o\u2026"
lastmod: '2024-03-13T22:44:47.043058-06:00'
model: gpt-4-0125-preview
summary: "Em C, os n\xFAmeros complexos s\xE3o suportados pela Biblioteca Padr\xE3\
  o, especificamente `<complex.h>`."
title: "Trabalhando com N\xFAmeros Complexos"
weight: 14
---

## Como Fazer:
Em C, os números complexos são suportados pela Biblioteca Padrão, especificamente `<complex.h>`. Para utilizá-los, declare variáveis com o tipo `double complex` (ou `float complex` para precisão simples). Veja como realizar operações básicas:

```c
#include <stdio.h>
#include <complex.h>

int main() {
    double complex z1 = 1.0 + 2.0*I; // Declara um número complexo 1+2i
    double complex z2 = 1.0 - 2.0*I; // Declara outro número complexo 1-2i
    
    // Adição
    double complex sum = z1 + z2;
    printf("Soma: %.2f + %.2fi\n", creal(sum), cimag(sum)); // Saída: Soma: 2.00 + 0.00i

    // Multiplicação
    double complex product = z1 * z2;
    printf("Produto: %.2f + %.2fi\n", creal(product), cimag(product)); // Saída: Produto: 5.00 + 0.00i

    // Conjugado Complexo
    double complex conjugate = conj(z1);
    printf("Conjugado de z1: %.2f + %.2fi\n", creal(conjugate), cimag(conjugate)); // Saída: Conjugado de z1: 1.00 - 2.00i
    
    // Magnitude
    double magnitude = cabs(z1);
    printf("Magnitude de z1: %.2f\n", magnitude); // Saída: Magnitude de z1: 2.24

    // Fase
    double phase = carg(z1);
    printf("Fase de z1: %.2f\n", phase); // Saída em radianos
    
    return 0;
}
```
Note que `I` é uma constante representando a unidade imaginária em `<complex.h>`. Funções como `creal()` e `cimag()` extraem as partes real e imaginária, respectivamente, enquanto `conj()` calcula o conjugado complexo. Para a magnitude e fase (argumento) de números complexos, `cabs()` e `carg()` são usadas.

## Aprofundamento
O suporte para números complexos em C é relativamente recente, tendo sido padronizado no C99. Antes disso, a aritmética de números complexos em C era trabalhosa, muitas vezes requerendo estruturas de dados e funções personalizadas. A inclusão de `<complex.h>` e os tipos de dados complexos proporcionaram um significativo impulso às capacidades da linguagem para aplicações científicas e de engenharia. No entanto, vale ressaltar que algumas linguagens, como Python, oferecem suporte mais intuitivo para números complexos por meio de tipos de dados embutidos e um conjunto mais rico de funções de biblioteca. Apesar disso, o desempenho e controle oferecidos por C o tornam uma escolha preferencial para tarefas de computação de alto desempenho, mesmo que isso signifique lidar com uma sintaxe um pouco mais verbosa para a aritmética complexa.
