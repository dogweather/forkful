---
title:                "C#: Écriture de tests"
simple_title:         "Écriture de tests"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi écrire des tests en programmation C#?

Écrire des tests est essentiel pour garantir la qualité de votre code et pour vous assurer que votre application fonctionne correctement. Les tests automatisés permettent de détecter rapidement les erreurs et de les corriger avant qu'elles ne deviennent un problème majeur.

## Comment écrire des tests en C#?

Il existe de nombreuses façons d'écrire des tests en C#. L'une des méthodes les plus courantes est d'utiliser le framework de tests intégré à Visual Studio, appelé Microsoft Unit Test Framework. Voici un exemple de code pour tester une fonction de calcul de moyenne:

```C#
// Définir les valeurs d'entrée
double[] valeurs = { 2.5, 3.5, 4.0 };

// Appeler la fonction à tester
double moyenne = CalculerMoyenne(valeurs);

// Comparer les résultats avec les valeurs attendues
Assert.AreEqual(3.333, moyenne);
```

Le code ci-dessus définit un tableau de valeurs à entrer dans la fonction `CalculerMoyenne` et utilise l'assertion `AreEqual` pour comparer la moyenne calculée avec la valeur attendue. Avec ce framework, vous pouvez facilement écrire des tests pour toutes les parties de votre code.

## Plongée en profondeur

Écrire des tests ne consiste pas seulement à vérifier si votre code fonctionne correctement, c'est aussi une façon de concevoir un code plus modulaire et facile à maintenir. En écrivant des tests unitaires, vous êtes obligé de découper votre code en petites parties bien définies, ce qui facilite la compréhension et la modification ultérieure.

En outre, les tests automatisés vous permettent de mieux gérer les modifications apportées à votre code. Si vous devez effectuer une modification importante, vous pouvez d'abord exécuter tous vos tests pour vous assurer que rien n'a été cassé. Si un test échoue, vous savez immédiatement quelles parties de votre code ont été affectées par la modification.

## Voir aussi

- [Introduction aux tests en C#](https://openclassrooms.com/en/courses/2818931-testez-votre-micro-service/2835213-principe-des-tests-unitaires)
- [Documentation Microsoft pour le framework de tests unitaires en C#](https://docs.microsoft.com/en-us/visualstudio/test/walkthrough-creating-and-running-unit-tests-for-managed-code)
- [Guide de développement de tests en C# par Microsoft](https://docs.microsoft.com/en-us/visualstudio/test/developing-unit-tests-for-managed-code)