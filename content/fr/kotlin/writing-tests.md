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

## Pourquoi

Si vous êtes développeur, vous avez certainement entendu parler des tests de code. Mais pourquoi est-il important de les écrire ? En un mot, les tests vous permettent de vérifier si votre code fonctionne correctement et évitent les bugs et les erreurs dans votre application. Cela peut vous faire gagner énormément de temps et d'efforts à long terme.

## Comment faire

Voici un exemple simple de test écrit en Kotlin pour une fonction qui calcule la somme de deux nombres :

```Kotlin

fun somme(nombre1: Int, nombre2: Int): Int {
    return nombre1 + nombre2
}

```

Pour tester cette fonction, nous allons utiliser la bibliothèque de tests unitaires intégrée à Kotlin, appelée "JUnit". Voici comment cela se présente :

```Kotlin

import org.junit.Test
import org.junit.Assert.assertEquals

class TestSomme {
    @Test
    fun sommeTest() {
        val somme = somme(5, 7)
        assertEquals(12, somme)
    }
}

```

Vous pouvez remarquer que nous avons importé deux éléments : `org.junit.Test`, qui nous permet d'annoter notre méthode avec `@Test` et `org.junit.Assert.assertEquals` qui nous permet d'utiliser la méthode `assertEquals` pour vérifier si notre résultat est bien égal à celui attendu. Nous créons ensuite une classe de test `TestSomme` avec une méthode `sommeTest` annotée avec `@Test`. À l'intérieur de cette méthode, nous appelons notre fonction `somme` avec les paramètres 5 et 7, et nous utilisons `assertEquals` pour vérifier si le résultat est bien égal à 12.

Pour exécuter ce test, vous pouvez soit exécuter la classe `TestSomme` directement, soit exécuter `sommeTest` en tant que test unitaire. Vous devriez voir un message de succès indiquant que votre test a bien été exécuté.

## Plongeons plus en profondeur

Maintenant que vous avez vu un exemple simple de test, vous pouvez vous demander comment écrire des tests plus complexes et comment organiser votre code de test. Dans la plupart des cas, vous pouvez suivre la même structure que dans notre exemple : importer les bibliothèques nécessaires, créer une classe de test et écrire des méthodes annotées avec `@Test` pour chaque fonction que vous voulez tester.

Il est également important de tester chaque fonction avec différents cas de test pour couvrir tous les scénarios possibles. Vous pouvez également utiliser des assertions plus complexes comme `assertTrue` ou `assertFalse` pour vérifier des conditions spécifiques.

Mais souvenez-vous que le but des tests n'est pas de couvrir chaque ligne de code, mais plutôt de cibler les parties les plus importantes de votre application et de s'assurer qu'elles fonctionnent correctement.

## Voir aussi

Pour en savoir plus sur les tests en Kotlin, vous pouvez consulter les ressources suivantes :

- [Documentation officielle de Kotlin sur les tests unitaires](https://kotlinlang.org/docs/reference/testing.html)
- [Article sur les meilleures pratiques de tests en Kotlin](https://www.javacodegeeks.com/2019/05/kotlin-unit-testing-best-practices.html)
- [Vidéo explicative sur les tests en Kotlin](https://www.youtube.com/watch?v=0Z9uN5qWTLQ)