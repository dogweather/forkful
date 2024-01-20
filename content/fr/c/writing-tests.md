---
title:                "Ecrire des tests"
html_title:           "C: Ecrire des tests"
simple_title:         "Ecrire des tests"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/writing-tests.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Écrire des tests est une pratique courante pour les programmeurs. Cela consiste à écrire du code supplémentaire pour vérifier que le code existant fonctionne correctement. Les programmeurs le font pour s'assurer que leur code n'a pas de bugs et qu'il fonctionne comme prévu.

## Comment faire :

Pour écrire des tests en C, il y a plusieurs étapes à suivre :

```C
#include <stdio.h>
#include <assert.h>

// Fonction à tester
int additionner(int a, int b) {
    return a + b;
}

int main() {
    // Tests avec assert
    assert(additionner(2, 2) == 4); // Le test réussit, rien ne s'affiche
    assert(additionner(5, 10) == 15); // Le test réussit, rien ne s'affiche
    assert(additionner(3, 6) == 9); // Le test échoue, un message d'erreur s'affiche
    return 0;
}
```

## Plongée en profondeur :

Les tests sont devenus une pratique courante dans le développement logiciel pour améliorer la qualité et la fiabilité du code. Avant l'avènement des tests automatisés, les programmeurs devaient tester manuellement leur code, ce qui était fastidieux et sujet à des erreurs. Les tests automatisés permettent d'économiser du temps et de détecter plus facilement les bugs.

Il existe différentes façons d'écrire des tests en C, telles que l'utilisation de bibliothèques de tests comme CUnit ou la mise en place de macros personnalisées pour faciliter l'écriture des tests.

## Voir aussi :

- [Tutoriel sur CUnit](https://triia.fr/articles/][https://triia.fr/articles/tutoriel-csqa-cunit/]