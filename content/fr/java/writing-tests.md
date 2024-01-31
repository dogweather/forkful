---
title:                "Rédaction de tests"
date:                  2024-01-19
html_title:           "Arduino: Rédaction de tests"
simple_title:         "Rédaction de tests"

category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Écrire des tests, c'est créer des scénarios pour vérifier que notre code fait ce qu'il est censé faire. On teste pour prévenir les bugs, garantir la qualité et simplifier les mises à jour.

## How to: (Comment faire : )
Utilise Java avec JUnit pour écrire des tests. Voici un exemple simple :

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class CalculatriceTest {
    @Test
    public void testAddition() {
        assertEquals(4, Calculatrice.additionner(2, 2));
    }
}

class Calculatrice {
    static int additionner(int a, int b) {
        return a + b;
    }
}
```

Après l'exécution du test :

```
Test passed (1 test, 1 passed, 0 failed, 0 skipped)
```

## Deep Dive (Plongée en profondeur)
Les tests sont un pilier du développement logiciel depuis des années. JUnit est un cadre de test largement utilisé pour Java. Il existe des alternatives comme TestNG ou Mockito pour les mocks. Un bon test est rapide, isolé et répétable. Optimisez les tests avec `@BeforeEach` pour la configuration et `@AfterEach` pour le nettoyage.

## See Also (Voir aussi)
- [JUnit 5 User Guide](https://junit.org/junit5/docs/current/user-guide/)
- [Mockito](https://site.mockito.org/)
- [Oracle's Java Tutorials - Testing](https://docs.oracle.com/javase/tutorial/junit/index.html)
