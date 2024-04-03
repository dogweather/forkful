---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:51.855289-07:00
description: "I numeri complessi sono costituiti da una parte reale e una immaginaria,\
  \ rappresentati come `a + bi` dove `i` \xE8 la radice quadrata di `-1`. I\u2026"
lastmod: '2024-03-13T22:44:43.906257-06:00'
model: gpt-4-0125-preview
summary: "I numeri complessi sono costituiti da una parte reale e una immaginaria,\
  \ rappresentati come `a + bi` dove `i` \xE8 la radice quadrata di `-1`."
title: Lavorare con i numeri complessi
weight: 14
---

## Come fare:
In C, i numeri complessi sono supportati dalla Libreria Standard, specificamente `<complex.h>`. Per utilizzarli, dichiara variabili con il tipo `double complex` (o `float complex` per la precisione singola). Ecco come eseguire operazioni di base:

```c
#include <stdio.h>
#include <complex.h>

int main() {
    double complex z1 = 1.0 + 2.0*I; // Dichiarazione di un numero complesso 1+2i
    double complex z2 = 1.0 - 2.0*I; // Dichiarazione di un altro numero complesso 1-2i
    
    // Addizione
    double complex sum = z1 + z2;
    printf("Somma: %.2f + %.2fi\n", creal(sum), cimag(sum)); // Output: Somma: 2.00 + 0.00i

    // Moltiplicazione
    double complex product = z1 * z2;
    printf("Prodotto: %.2f + %.2fi\n", creal(product), cimag(product)); // Output: Prodotto: 5.00 + 0.00i

    // Coniugato Complesso
    double complex conjugate = conj(z1);
    printf("Coniugato di z1: %.2f + %.2fi\n", creal(conjugate), cimag(conjugate)); // Output: Coniugato di z1: 1.00 - 2.00i
    
    // Magnitudine
    double magnitudine = cabs(z1);
    printf("Magnitudine di z1: %.2f\n", magnitudine); // Output: Magnitudine di z1: 2.24

    // Fase
    double fase = carg(z1);
    printf("Fase di z1: %.2f\n", fase); // Output in radianti
    
    return 0;
}
```
Nota che `I` è una costante che rappresenta l'unità immaginaria in `<complex.h>`. Funzioni come `creal()` e `cimag()` estraggono rispettivamente le parti reale e immaginaria, mentre `conj()` calcola il coniugato complesso. Per la magnitudine e la fase (argomento) dei numeri complessi, si usano `cabs()` e `carg()`.

## Approfondimento
Il supporto per i numeri complessi in C è relativamente recente, essendo stato standardizzato nel C99. Prima di ciò, l'aritmetica dei numeri complessi in C era ingombrante, spesso richiedendo strutture dati e funzioni personalizzate. L'inclusione di `<complex.h>` e i tipi di dati complessi hanno fornito un significativo impulso alle capacità del linguaggio per le applicazioni scientifiche e ingegneristiche. Tuttavia, vale la pena notare che alcuni linguaggi, come Python, offrono un supporto più intuitivo per i numeri complessi tramite tipi di dati integrati e un set più ricco di funzioni di libreria. Nonostante ciò, le prestazioni e il controllo offerti da C rendono una scelta preferenziale per compiti di calcolo ad alte prestazioni, anche se ciò significa avere a che fare con una sintassi leggermente più verbosa per l'aritmetica complessa.
