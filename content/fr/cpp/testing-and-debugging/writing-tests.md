---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:53.964526-07:00
description: "\xC9crire des tests en C++ implique de cr\xE9er des programmes petits\
  \ et autonomes qui v\xE9rifient automatiquement le comportement de sections de votre\
  \ base de\u2026"
lastmod: '2024-03-13T22:44:58.165771-06:00'
model: gpt-4-0125-preview
summary: "\xC9crire des tests en C++ implique de cr\xE9er des programmes petits et\
  \ autonomes qui v\xE9rifient automatiquement le comportement de sections de votre\
  \ base de code."
title: "R\xE9daction de tests"
weight: 36
---

## Comment faire :


### Utilisation du cadre de test Google Test
L'une des bibliothèques tierces les plus populaires pour écrire des tests en C++ est Google Test. Tout d'abord, vous devrez installer Google Test et le lier à votre projet. Une fois configuré, vous pouvez commencer à écrire des cas de test.

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

Enregistrez le code dans un fichier, et compilez-le avec le compilateur g++, en liant la bibliothèque Google Test. Si tout est correctement configuré, l'exécution du programme exécutable résultant lancera le test, et si la fonction `add` fonctionne comme prévu, vous verrez quelque chose comme :

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

### Utilisation de Catch2
Un autre cadre de test populaire pour C++ est Catch2. Il a une syntaxe plus simple et ne nécessite généralement pas de liaison contre une bibliothèque (uniquement d'en-tête). Voici un exemple de comment écrire un test simple avec Catch2 :

```cpp
#define CATCH_CONFIG_MAIN  // Cela indique à Catch de fournir un main() - ne faites cela que dans un fichier cpp
#include <catch.hpp>

int multiply(int a, int b) {
    return a * b;
}

TEST_CASE( "Les entiers sont multipliés", "[multiply]" ) {
    REQUIRE( multiply(2, 3) == 6 );
}
```

En compilant et exécutant ce test, Catch2 fournit une sortie claire indiquant si le test a réussi ou échoué, avec toutes les informations nécessaires pour déboguer les échecs :

```
===============================================================================
All tests passed (1 assertion in 1 test case)
```

Ces exemples montrent comment l'intégration de cadres de test dans votre flux de travail de développement C++ peut améliorer de manière significative la fiabilité et la maintenabilité de votre code.
