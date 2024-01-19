---
title:                "Stampa dell'output di debug"
html_title:           "Arduino: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/printing-debug-output.md"
---

{{< edit_this_page >}}

# Stampa dell'output di debug in C

## Che Cos'è e Perché?

La stampa dell'output di debug è un metodo usato dai programmatori per monitorare e analizzare lo stato del loro programma durante l'esecuzione. È utile per identificare e correggere errori nel codice (debugging).

## Come Fare:

Un esempio semplice usando `printf` dal regno standard `stdio.h`:

```C
#include <stdio.h>

int main() {
    int a = 5;
    printf("Il valore di a è: %d\n", a);
    return 0;
}
```

Output:

```
Il valore di a è: 5
```

## Approfondimenti

Per la stampa dell'output di debug in C, la funzione `printf` è la più utilizzata dai tempi antichi, grazie alla sua flessibilità e semplicità. Tuttavia, ci sono alternative come `fprintf` per scrivere su un file di log, o `sprintf` per scrivere su una stringa.

Sotto il cofano, `printf` converte i tipi di dati specificati in stringhe e le scrive all'STDOUT, il che può comportare un overhead di performance.

Inoltre, per un approccio più moderno e sicuro, si può considerare l'uso del modulo `syslog` disponibile nelle librerie GNU, che offre funzioni di logging più robuste.

## Approfondisci

Qui ci sono alcuni link per ulteriori approfondimenti:

- [Documentazione di `printf`](http://www.cplusplus.com/reference/cstdio/printf/)
- [Documentazione di `syslog`](https://www.gnu.org/software/libc/manual/html_node/System-Logger.html)
- [`printf` vs `fprintf` vs `sprintf`](https://stackoverflow.com/questions/7124882/what-is-the-difference-between-printf-fprintf-and-sprintf)