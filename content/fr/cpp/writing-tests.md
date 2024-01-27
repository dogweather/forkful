---
title:                "Rédaction de tests"
date:                  2024-01-19
html_title:           "Arduino: Rédaction de tests"
simple_title:         "Rédaction de tests"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Écrire des tests, c’est créer du code pour vérifier que d'autres morceaux de code fonctionnent comme prévu. On fait ça pour éviter les bugs, garantir la qualité, et simplifier l'amélioration du code plus tard.

## How to: (Comment faire :) 

On va utiliser le framework de tests Google Test pour un exemple simple :

```cpp
#include <gtest/gtest.h>

int somme(int a, int b) {
    return a + b;
}

TEST(SommeTest, Positifs) {
    EXPECT_EQ(7, somme(3, 4));
}

TEST(SommeTest, Negatifs) {
    EXPECT_EQ(-2, somme(-1, -1));
}

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```
Sortie attendue après compilation et exécution :
```
[==========] Running 2 tests from 1 test suite.
[----------] Global test environment set-up.
[----------] 2 tests from SommeTest
[ RUN      ] SommeTest.Positifs
[       OK ] SommeTest.Positifs (0 ms)
[ RUN      ] SommeTest.Negatifs
[       OK ] SommeTest.Negatifs (0 ms)
[----------] 2 tests from SommeTest (0 ms total)

[----------] Global test environment tear-down
[==========] 2 tests from 1 test suite ran. (1 ms total)
[  PASSED  ] 2 tests.
```

## Deep Dive (Plongée Profonde)

Le test de logiciels date de la création des premiers programmes. Faire le test manuellement est répétitif et sujet à erreurs, alors on a créé des frameworks de tests. En plus de Google Test pour C++, y'a des alternatives comme Boost.Test et Catch2. Les bonnes pratiques recommandent l’utilisation des tests unitaires, qui vérifient des petites parties du code, et des tests d'intégration, pour les interactions entre composants.

## See Also (Voir Aussi)

- La doc de Google Test : https://github.com/google/googletest
- Intro à Boost.Test : https://www.boost.org/doc/libs/1_75_0/libs/test/doc/html/index.html
- Catch2 pour débutants : https://github.com/catchorg/Catch2
