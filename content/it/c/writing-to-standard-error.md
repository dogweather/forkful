---
title:                "C: Scrittura su errore standard"
simple_title:         "Scrittura su errore standard"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere a standard error è un modo efficace per visualizzare messaggi di errore o di debug durante l'esecuzione di un programma. Può anche essere utile per registrare informazioni importanti durante l'esecuzione del programma.

## Come Fare

Per scrivere a standard error in un programma in linguaggio C, è necessario includere la libreria `<stdio.h>` e utilizzare la funzione `fprintf` con il primo parametro impostato su `stderr`. Ad esempio:

```C
#include <stdio.h>

int main() {

  int numero = 10;
  
  // stampa il messaggio di errore a standard error
  fprintf(stderr, "Il valore di numero è: %d\n", numero);
  
  return 0;
}
```

L'output di questo codice sarà:

`Il valore di numero è: 10`

Si noti che il messaggio di errore viene stampato a video solo se si utilizza un compilatore che supporta il concetto di standard error. In caso contrario, verrà stampato a standard output.

## Approfondimento

La funzione `fprintf` è molto simile alla funzione `printf` in quanto entrambe vengono utilizzate per stampare un messaggio formattato su uno stream di output. Tuttavia, la differenza principale è che `printf` scrive su standard output, mentre `fprintf` può essere utilizzata per scrivere su qualsiasi stream, inclusi `stdout` e `stderr`.

Inoltre, è importante notare che a differenza della funzione `printf`, `fprintf` richiede un parametro aggiuntivo prima della stringa di formato con il nome dello stream su cui si vuole scrivere.

## Vedi Anche

- The difference between fprintf, printf, and sprintf (https://www.quora.com/What-is-the-difference-between-printf-spring-and-fprintf)
- C programming tutorial (https://www.programiz.com/c-programming)