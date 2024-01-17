---
title:                "Écriture de tests"
html_title:           "Kotlin: Écriture de tests"
simple_title:         "Écriture de tests"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & pourquoi?

Écrire des tests est un processus dans lequel les programmeurs écrivent du code spécialement conçu pour tester leur propre code. Cela garantit que leur code fonctionne correctement et évite les bugs lors de l'exécution.

## Comment faire:

```Kotlin
// Exemple 1:
fun addition(a: Int, b: Int) = a + b 

``` 
Output:
```Kotlin
addition(5, 7) 
// 12 
```

```Kotlin
// Exemple 2: 
fun estPair(a: Int): Boolean = 
    if (a % 2 == 0)
        true
    else
        false
```

Output:
```Kotlin
estPair(3)
// false 

estPair(8)
// true
``` 

## Plongée en profondeur:

Écrire des tests n'est pas un concept nouveau, car il a été introduit dans les années 1950 avec le développement des premiers ordinateurs. De nos jours, il existe différents types de tests tels que les tests unitaires, les tests d'intégration et les tests fonctionnels. Il existe également des alternatives comme le développement piloté par les tests (Test-Driven Development - TDD) et le développement piloté par le comportement (Behavior-Driven Development - BDD).

Pour implémenter des tests en Kotlin, il existe plusieurs frameworks populaires tels que JUnit, Spek et Kotest.

## Voir aussi:

- [Introduction aux tests en Kotlin](https://www.kotlindevelopment.com/testing-kotlin/)
- [Tutoriel JUnit pour les tests en Kotlin](https://medium.com/@jmathieu/tutoriel-junit-pour-tester-ses-applications-kotlin-91d2dc539c1c)
- [Documentation officielle de Kotest](https://kotest.io/docs/framework/introduction.html)