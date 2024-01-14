---
title:                "Kotlin: Écrire des tests"
simple_title:         "Écrire des tests"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi 
Ecrire des tests est essentiel pour maintenir la qualité et la fiabilité de votre code Kotlin. Cela peut vous faire gagner du temps et éviter des erreurs coûteuses à long terme en vous assurant que votre code fonctionne comme prévu.

## Comment faire
Voici un exemple simple de test unitaire en Kotlin pour une fonction qui calcule la moyenne de deux nombres :

```Kotlin

fun average(a: Int, b: Int): Int {
    return (a + b) / 2
}

/**
* Test pour voir si la moyenne de 2 et 4 est égale à 3
*/
@Test
fun testAverage() {
    assertEquals(3, average(2, 4))
}

```

En utilisant la fonction `assertEquals()` du framework de test JUnit, nous pouvons vérifier si la sortie de notre fonction correspond à notre attente. Dans cet exemple, si la moyenne calculée n'est pas égale à 3, le test échouera et nous indiquera qu'il y a un problème dans notre code.

De plus, il est important de tester différents scénarios de votre code pour vous assurer qu'il fonctionne correctement dans toutes les situations possibles. En utilisant des frameworks de test comme JUnit ou Mockito, vous pouvez facilement simuler des entrées et des comportements spécifiques pour tester votre code de manière plus approfondie.

## Plongée en profondeur
Écrire des tests peut sembler fastidieux et prendre du temps, mais cela peut vous faire gagner beaucoup de temps et d'efforts à long terme. En ayant une suite de tests solide en place, vous pouvez être plus confiant lors de la modification ou de l'ajout de fonctionnalités à votre code, car vous saurez rapidement si vous avez introduit des bugs ou des erreurs.

De plus, les tests automatisés peuvent également être utiles pour la refactoring de code. Lorsque vous apportez des modifications à votre code, vous pouvez exécuter vos tests pour vous assurer que tout fonctionne toujours correctement et que vous n'avez pas introduit de nouvelles erreurs. Si certains tests échouent, cela vous indique immédiatement quelles parties du code ont été affectées par vos modifications.

## Voir aussi
- [Tutoriel Kotlin pour débutants](https://kotlinlang.org/docs/tutorials/getting-started.html)
- [Guide de test Kotlin par Baeldung](https://www.baeldung.com/kotlin/testing)
- [Documentation de JUnit pour Kotlin](https://junit.org/junit5/docs/current/user-guide/#writing-tests)