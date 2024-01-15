---
title:                "Scrivere test"
html_title:           "C: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/writing-tests.md"
---

{{< edit_this_page >}}

##Perché

Scrivere test può sembrare una parte noiosa e tediosa del processo di sviluppo software, ma in realtà è uno strumento fondamentale per garantire la qualità del codice e facilitare il processo di debugging. Inoltre, scrivere test può aiutare a risparmiare tempo e risorse in futuro, poiché individua eventuali errori o bug fin dalla fase di sviluppo.

##Come fare

Per scrivere test efficaci in C, è importante comprendere i concetti di base del linguaggio e le librerie standard. Inoltre, ci sono alcune buone pratiche da seguire per assicurarsi che i test siano affidabili e di facile manutenzione. Ecco un esempio di test di una funzione che calcola il massimo tra due numeri:

```C
#include <stdio.h>

int max(int a, int b);

int main() {
    int result = max(5, 10);
    printf("Il massimo è: %d", result);
    return 0;
}

int max(int a, int b) {
    int max = a > b ? a : b; // operatore ternario per semplificare il codice
    return max;
}
```

In questo esempio, viene definita una funzione `max` che prende in input due interi e restituisce il maggiore dei due. Nella funzione `main`, viene chiamata la funzione `max` con due valori e il risultato viene stampato a video. Per creare il test di questa funzione, è possibile utilizzare una libreria di unit testing come `cmocka` o `minunit`:

```C
#include <stdio.h>
#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>

int max(int a, int b);

static void max_test(void **state) {
    assert_int_equal(max(5, 10), 10);
    assert_int_equal(max(-5, 0), 0);
}

int main() {
    const struct CMUnitTest tests[] = {
            cmocka_unit_test(max_test),
    };
    return cmocka_run_group_tests(tests, NULL, NULL);
}

int max(int a, int b) {
    int max = a > b ? a : b;
    return max;
}
```

In questo caso, la libreria `cmocka` viene utilizzata per definire un test chiamato `max_test`, che verifica se la funzione `max` restituisce il valore corretto per due input diversi. Per eseguire il test, è necessario compilare il codice con la libreria e avviare il programma, che restituirà un resoconto dettagliato su eventuali errori o fallimenti dei test.

##Approfondimento

Scrivere test accurati e completi può richiedere una certa quantità di tempo ed energia, ma i suoi vantaggi sono molteplici. Inoltre, i test possono essere utilizzati in diversi modi, come per la verifica dell'integrità del codice durante il processo di sviluppo o per la validazione del software prima della distribuzione. Inoltre, un buon set di test ben strutturato può fungere da documentazione aggiuntiva per il codice, fornendo esempi di utilizzo delle funzioni.

##Vedi anche

- [Guida alle buone pratiche di testing in C](https://www.seguridad.unam.mx/media/archivo/proyecto3/good-practices-for-unit-testing-in-c-c-j-owner-s-guide-en-us.pdf)
- [Esempi di test in C con la libreria Unity](https://www.throwtheswitch.org/unity)
- [Tutorial su come utilizzare la libreria cmocka per fare unit testing in C](https://blog.jgc.org/2019/01/a-tutorial-on-how-to-do-unit-testing-of.html)