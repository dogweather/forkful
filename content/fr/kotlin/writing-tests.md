---
title:                "Rédaction de tests"
aliases:
- fr/kotlin/writing-tests.md
date:                  2024-02-03T19:30:57.393761-07:00
model:                 gpt-4-0125-preview
simple_title:         "Rédaction de tests"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Écrire des tests en Kotlin consiste à créer des extraits de code qui valident automatiquement la correction fonctionnelle de vos modules logiciels, en s'assurant qu'ils fonctionnent comme prévu. Les programmeurs le font pour détecter les bogues tôt, faciliter le refactoring du code et fournir de la documentation sur le fonctionnement prévu des composants logiciels.

## Comment faire :

Kotlin prend en charge le développement piloté par les tests avec divers frameworks, les plus populaires étant JUnit, Kotest, et MockK pour le mocking. Voici un exemple simple utilisant JUnit :

```kotlin
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class CalculatorTest {

    @Test
    fun `ajoute deux nombres`() {
        val calculator = Calculator()
        val result = calculator.add(2, 3)
        assertEquals(5, result)
    }
}

class Calculator {
    fun add(a: Int, b: Int): Int = a + b
}
```

**Sortie d'exemple**

```text
Test réussi.
```

Pour une approche de test plus sophistiquée utilisant Kotest, qui offre un style d'écriture de test Kotlin plus idiomatique, voir l'exemple ci-dessous :

```kotlin
import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe

class CalculatorSpec : StringSpec({
    "ajouter 2 et 3 devrait retourner 5" {
        val calculator = Calculator()
        calculator.add(2, 3) shouldBe 5
    }
})
```

Utiliser MockK pour tester avec des mocks :

```kotlin
import io.mockk.every
import io.mockk.mockk
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class ServiceTest {

    private val repository = mockk<Repository>()
    private val service = Service(repository)

    @Test
    fun `obtenir des données retourne des données simulées`() {
        every { repository.getData() } returns "Données Simulées"

        val result = service.getData()

        assertEquals("Données Simulées", result)
    }
}

class Service(private val repository: Repository) {
    fun getData(): String = repository.getData()
}

interface Repository {
    fun getData(): String
}
```

**Sortie d'exemple**

```text
Test réussi.
```

Ces exemples illustrent les bases de l'écriture de tests unitaires en Kotlin. Au fur et à mesure que votre application grandit, envisagez d'explorer des techniques et des outils de test plus avancés fournis par chaque framework.
