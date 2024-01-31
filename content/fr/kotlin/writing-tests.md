---
title:                "Rédaction de tests"
date:                  2024-01-19
html_title:           "Arduino: Rédaction de tests"
simple_title:         "Rédaction de tests"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Écrire des tests, c'est vérifier que chaque partie de votre code fonctionne comme prévu. Les développeurs font ça pour éviter les bugs, simplifier les modifications et assurer une qualité fiable.

## Comment faire :

```Kotlin
// Ajoutons JUnit à notre projet Gradle
dependencies {
    testImplementation("org.junit.jupiter:junit-jupiter:5.7.1")
}

// Un test simple avec JUnit 5
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.*

class CalculatriceTest {

    @Test
    fun `test addition`() {
        val resultat = Calculatrice.additionner(3, 4)
        assertEquals(7, resultat, "3 + 4 doit être égal à 7")
    }
}

// La classe Calculatrice
class Calculatrice {
    companion object {
        fun additionner(a: Int, b: Int) = a + b
    }
}
```

Sortie :

```
Test passed.
```

## Plongée profonde

Historiquement, JUnit est le framework de test dominant en Java, et par extension, en Kotlin. Alternativement, Kotlin offre aussi Kotest et Spek. Ces cadres insistent sur la lisibilité et l'idiomatisme Kotlin. Les détails d'implémentation comptent : une bonne isolation empêche les tests d'interférer entre eux.

## Voir aussi :

- Documentation JUnit 5 : [https://junit.org/junit5/docs/current/user-guide/](https://junit.org/junit5/docs/current/user-guide/)
- Kotest : [https://kotest.io/](https://kotest.io/)
- Un aperçu de Spek : [https://www.spekframework.org/](https://www.spekframework.org/)
