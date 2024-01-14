---
title:                "C: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/writing-tests.md"
---

{{< edit_this_page >}}

## Perché scrivere test in C?
Scrivere test può sembrare una pratica lunga e noiosa, ma ha numerosi vantaggi. Innanzitutto, aiuta a individuare eventuali bug o errori nel codice prima che possano causare problemi più grandi. Inoltre, i test possono essere riutilizzati e automatizzati per risparmiare tempo e sforzo nelle fasi di sviluppo successive.

## Come scrivere test in C
Per scrivere test in C, è necessario utilizzare una libreria di unit testing come CUnit o Unity. Queste librerie forniscono funzionalità e strumenti per creare e gestire i test in modo semplice ed efficace.

Ecco un esempio di codice che utilizza CUnit per testare una funzione che calcola il quadrato di un numero:
```
// Inclusione delle librerie necessarie
#include <CUnit/Basic.h>
#include <stdio.h>
#include "square.h" // File contenente la definizione della funzione "square"

// Definizione del test
void testSquare(void) {
    // Valore di input
    int input = 5;
    // Valore atteso
    int expected = 25;
    // Richiamo della funzione da testare
    int result = square(input);
    // Confronto tra il risultato ottenuto e quello atteso
    CU_ASSERT_EQUAL(result, expected);
}

// Funzione di inizializzazione delle suite di test
int init_suite(void) {
    return 0;
}

// Funzione di pulizia delle suite di test
int clean_suite(void) {
    return 0;
}

// Funzione main che avvia l'esecuzione dei test
int main() {
    // Inizializzazione della suite di test
    CU_pSuite pSuite = NULL;
    // Creazione della suite di test
    pSuite = CU_add_suite("Square Test", init_suite, clean_suite);
    // Aggiunta del test alla suite
    CU_add_test(pSuite, "testSquare", testSquare);
    // Avvio dell'esecuzione dei test
    CU_basic_run_tests();
    // Pulizia delle suite di test
    CU_cleanup_registry();
    return CU_get_error();
}
```
Questo codice definisce un test per la funzione "square" e lo esegue utilizzando la libreria CUnit. Una volta eseguito il test, si otterrà un output come questo:
```
CUnit - A unit testing framework for C

Suite: Square Test
  Test: testSquare ...passed

Run Summary:    Type  Total   Ran Passed Failed Inactive
              suites      1     1    n/a      0        0
              tests       1     1      1      0        0
              asserts     1     1      1      0      n/a

Elapsed time =    0.000 seconds
```

## Approfondimenti sui test in C
Scrivere test può essere un processo complesso, ma esistono diverse best practice che possono aiutare a semplificarlo. Ad esempio, è importante scrivere test che siano facilmente ripetibili e che si concentrino sulle funzionalità più critiche del codice. Inoltre, è consigliato monitorare regolarmente i test per garantire che siano ancora validi e aggiornarli quando il codice viene modificato.

## Vedi anche
- [Guida alla scrittura di test con CUnit](http://cunit.sourceforge.net/doc/index.html)
- [Libreria di test Unity per C](https://github.com/ThrowTheSwitch/Unity)
- [Best practice per la scrittura di test in C](https://begriffs.com/posts/2017-04-13-designing-a-c-testing-framework.html)