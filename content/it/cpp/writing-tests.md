---
title:                "Scrivere test"
date:                  2024-01-19
html_title:           "Arduino: Scrivere test"
simple_title:         "Scrivere test"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (Che cosa e perché?)
Scrivere test verifica che il codice fa quello che deve. Si fa per trovare errori prima che diventino problemi e per assicurarsi che modifiche future non rompano le funzionalità esistenti.

## How to: (Come fare)
Ecco un esempio base di un test con Google Test, una popolare libreria di C++.
```C++
#include <gtest/gtest.h>

int Somma(int a, int b) {
    return a + b;
}

TEST(TestSomma, Positivi) {
    EXPECT_EQ(5, Somma(2, 3));
}

TEST(TestSomma, Negativi) {
    EXPECT_EQ(-5, Somma(-2, -3));
}

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```
Output di esempio:
```
[ RUN      ] TestSomma.Positivi
[       OK ] TestSomma.Positivi
[ RUN      ] TestSomma.Negativi
[       OK ] TestSomma.Negativi
```

## Deep Dive (Approfondimento)
I test automatici esistono dagli anni '60. Alternative a Google Test includono Boost.Test e Catch2. Importante è l'uso degli asserts (`ASSERT_*` per terminare subito il test se fallisce, `EXPECT_*` per continuare a testare gli altri casi).

## See Also (Vedi anche)
- Google Test: https://github.com/google/googletest
- Documentazione di Boost.Test: https://www.boost.org/doc/libs/release/libs/test/
- Introduzione a Catch2: https://github.com/catchorg/Catch2
