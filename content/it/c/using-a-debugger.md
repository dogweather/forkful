---
title:                "Utilizzo di un debugger"
date:                  2024-01-26T03:47:27.333096-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzo di un debugger"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/using-a-debugger.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Un debugger è uno strumento che permette di ispezionare il proprio codice C mentre viene eseguito, passo dopo passo, per scovare bug. I programmatori utilizzano i debugger per comprendere come si comporta il loro codice, correggere problemi e ottimizzare le prestazioni senza procedere per tentativi.

## Come fare:
Supponiamo che stai lavorando a un semplice programma in C che calcola il fattoriale di un numero, ma c'è un inconveniente. Per utilizzare un debugger come `gdb` (GNU Debugger), prima compila con il flag `-g` per includere le informazioni di debug:

```c
// compila con: gcc factorial.c -o factorial -g
#include <stdio.h>

long factorial(int n) {
    if (n < 0) return 0; // Un semplice controllo per l'input negativo
    long result = 1;
    while (n > 1)
        result *= n--;
    return result;
}

int main() {
    int number = 5;
    long result = factorial(number);
    printf("Il fattoriale di %d è %ld\n", number, result);
    return 0;
}
```

Poi eseguilo in gdb:

```shell
$ gdb ./factorial
```

Imposta un breakpoint alla funzione `factorial` ed esegui il programma:

```gdb
(gdb) break factorial
(gdb) run
```

Quando si raggiunge il breakpoint, procedi passo dopo passo usando `next` o `n` e ispeziona le variabili con `print` o `p`:

```gdb
(gdb) next
(gdb) print result
$1 = 1
```

L'output di esempio fornirà valori in tempo reale e il flusso di esecuzione del programma.

## Approfondimento
I debugger esistono dagli anni '60, evolvendosi da semplici monitor a complesse applicazioni basate su GUI. Il debugging basato su stampe era comune prima dello sviluppo di debugger maturi. Alternative a `gdb` includono `lldb`, `dbx`, o debugger integrati in IDE come quelli di Visual Studio o CLion.

Quando si tratta di debugger, l'implementazione varia—alcuni possono catturare errori di runtime, esaminare la memoria, o addirittura invertire l'esecuzione di un programma. `gdb` può collegarsi a processi già in esecuzione, consentendo il debugging di software già in funzione, un vantaggio per la correzione di bug su sistemi in uso.

## Vedi anche
- GNU Debugger (GDB): https://www.gnu.org/software/gdb/documentation/
- Debugging con GDB: https://sourceware.org/gdb/current/onlinedocs/gdb
- Debugger LLDB: https://lldb.llvm.org/use/tutorial.html
- Tecniche di Debugging in C: http://www.cprogramming.com/debugging/debugging.html
