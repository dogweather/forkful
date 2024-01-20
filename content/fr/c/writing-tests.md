---
title:                "Rédaction de tests"
html_title:           "Arduino: Rédaction de tests"
simple_title:         "Rédaction de tests"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi ?)
Écrire des tests consiste à créer un ensemble de cas pour vérifier que le code fonctionne comme prévu. Les développeurs écrivent des tests pour détecter les bugs tôt, simplifier les modifications et assurer une base de code de qualité.

## How to (Comment faire : )
Voici un exemple simple de test unitaire en C en utilisant MinUnit, un micro-framework de test unitaire minimaliste :

```C
#include <stdio.h>

// MinUnit test framework
#define mu_assert(message, test) do { if (!(test)) return message; } while (0)
#define mu_run_test(test) do { char *message = test(); tests_run++; \
                                if (message) return message; } while (0)
int tests_run = 0;

// Fonction à tester
int additionner(int a, int b) {
    return a + b;
}

// Test
static char * test_addition() {
    mu_assert("Erreur, additionner(1,1) != 2", additionner(1,1) == 2);
    return 0;
}

// Routine principale des tests
static char * tous_les_tests() {
    mu_run_test(test_addition);
    return 0;
}

int main(int argc, char **argv) {
    char *resultat = tous_les_tests();
    if (resultat != 0) {
        printf("%s\n", resultat);
    } else {
        printf("Tous les tests passent.\n");
    }
    printf("Tests effectués: %d\n", tests_run);

    return resultat != 0;
}
```

Sortie attendue si le test réussit :

```
Tous les tests passent.
Tests effectués: 1
```

## Deep Dive (Plongée en profondeur)
Le test de logiciel existe depuis les années 1950. Avec l'évolution de la programmation, les tests ont aussi progressé. Des alternatives comme CUnit, Check, et cmocka offrent plus de fonctionnalités que MinUnit. L'écriture de tests unitaires en C s’appuie souvent sur des macros et des fonctions d'assertion pour valider les résultats.

## See Also (Voir aussi)
- [CUnit](http://cunit.sourceforge.net/)
- [Check](https://libcheck.github.io/check/)
- [cmocka](https://cmocka.org/)
- [Test-Driven Development](https://en.wikipedia.org/wiki/Test-driven_development)