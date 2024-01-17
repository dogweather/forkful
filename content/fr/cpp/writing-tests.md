---
title:                "Écrire des tests"
html_title:           "C++: Écrire des tests"
simple_title:         "Écrire des tests"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?
Ecrire des tests en programmation, c'est vérifier que le code fonctionne correctement en simulant son exécution. Les programmeurs le font pour garantir la qualité et la fiabilité de leur code, ainsi que pour détecter et corriger les erreurs dès le début du processus de développement.

## Comment faire:
Voici un exemple de code en C++ pour illustrer comment écrire des tests unitaires avec la bibliothèque Catch2:

```C++
#include <iostream>
#include "catch.hpp"

int square(int x) { /* Fonction à tester */ }

TEST_CASE("Fonction de test pour le carré", "[square]") {
    REQUIRE(square(2) == 4);
    REQUIRE(square(0) == 0);
    REQUIRE(square(-3) == 9);
}
```
Output:
```
===============================================================================
All tests passed (3 assertion in 1s)
```

## Plongée en profondeur:
Ecrire des tests n'est pas une pratique nouvelle en programmation, mais elle est devenue de plus en plus populaire avec l'avènement des méthodologies agiles et de la programmation orientée test. D'autres alternatives existent également pour écrire des tests tels que Google Test et Boost.Test. L'implémentation de tests unitaires peut également être réalisée avec des outils de développement tels Visual Studio et Eclipse.

## Voir aussi:
- [Catch2 documentation](https://github.com/catchorg/Catch2)
- [Google Test](https://github.com/google/googletest)
- [Boost.Test](https://www.boost.org/doc/libs/1_76_0/libs/test/doc/html/)
- [Introduction aux tests unitaires en C++](https://www.youtube.com/watch?v=ouGmVU7_yAM) (vidéo)