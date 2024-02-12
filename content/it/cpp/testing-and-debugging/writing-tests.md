---
title:                "Scrivere test"
aliases:
- it/cpp/writing-tests.md
date:                  2024-02-03T19:29:58.917894-07:00
model:                 gpt-4-0125-preview
simple_title:         "Scrivere test"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?

Scrivere test in C++ implica creare piccoli programmi autocontenuti che verificano automaticamente il comportamento di sezioni del tuo codice. I programmatori fanno ciò per assicurarsi che il loro codice funzioni come previsto, per prevenire regressioni (cioè, nuove modifiche che rompono funzionalità esistenti) e per facilitare la manutenibilità dei codici nel tempo.

## Come fare:

### Utilizzando Google Test Framework

Una delle librerie di terze parti più popolari per scrivere test in C++ è Google Test. Prima di tutto, dovrai installare Google Test e collegarlo al tuo progetto. Una volta configurato, puoi iniziare a scrivere casi di test.

```cpp
#include <gtest/gtest.h>

int add(int a, int b) {
    return a + b;
}

TEST(TestSuiteName, TestName) {
    EXPECT_EQ(3, add(1, 2));
}

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

Salva il codice in un file e compilalo con il compilatore g++, collegando la libreria Google Test. Se tutto è impostato correttamente, eseguendo l'eseguibile risultante verrà eseguito il test e, se la funzione `add` funziona come previsto, vedrai qualcosa del tipo:

```
[==========] Running 1 test from 1 test suite.
[----------] Global test environment set-up.
[----------] 1 test from TestSuiteName
[ RUN      ] TestSuiteName.TestName
[       OK ] TestSuiteName.TestName (0 ms)
[----------] 1 test from TestSuiteName (0 ms total)

[==========] 1 test from 1 test suite ran. (1 ms total)
[  PASSED  ] 1 test.
```

### Utilizzando Catch2

Un altro framework di test popolare per C++ è Catch2. Ha una sintassi più semplice e di solito non richiede il collegamento a una libreria (solo header). Ecco un esempio di come scrivere un semplice test con Catch2:

```cpp
#define CATCH_CONFIG_MAIN  // Questo dice a Catch di fornire un main() - farlo solo in un file cpp
#include <catch.hpp>

int multiply(int a, int b) {
    return a * b;
}

TEST_CASE("I numeri interi vengono moltiplicati", "[multiply]") {
    REQUIRE(multiply(2, 3) == 6);
}
```

Compilando ed eseguendo questo test, Catch2 fornisce un output chiaro che indica se il test è passato o fallito, insieme a tutte le informazioni necessarie per debuggare i fallimenti:

```
===============================================================================
Tutti i test passati (1 affermazione in 1 caso di test)
```

Questi esempi mostrano come l'integrazione di framework di test nel tuo flusso di lavoro di sviluppo C++ possa migliorare significativamente l'affidabilità e la manutenibilità del tuo codice.
