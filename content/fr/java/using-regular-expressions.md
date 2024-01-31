---
title:                "Utilisation des expressions régulières"
date:                  2024-01-19
simple_title:         "Utilisation des expressions régulières"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?

Utiliser des expressions régulières (regex) permet de chercher des motifs de texte complexes. Les développeurs s'en servent pour valider, rechercher, et manipuler des données textuelles efficacement.

## How to:

```java
import java.util.regex.*;

public class RegexExample {
    public static void main(String[] args) {
        // Créer un pattern pour détecter des adresses email valides
        Pattern pattern = Pattern.compile("[a-zA-Z0-9._%-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,4}");
        Matcher matcher = pattern.matcher("contactez-moi sur monmail@exemple.com ou admin@mailserver.fr");

        while (matcher.find()) {
            System.out.println("Email trouvé: " + matcher.group());
        }
    }
}
```
Sortie:
```
Email trouvé: monmail@exemple.com
Email trouvé: admin@mailserver.fr
```

## Deep Dive

Les expressions régulières existent depuis les années 1950, liées aux théories de l'automate de Stephen Kleene. JDK (Java Development Kit) a intégré le support de regex à partir de la version 1.4. Bien que puissantes, les regex ne sont pas toujours la solution optimale : elles peuvent être moins lisibles et plus lentes que d'autres méthodes, comme des parsers dédiés. Java utilise le moteur de regex NFA (Non-deterministic Finite Automaton), qui gère bien les cas complexes, mais avec une performance parfois non prédictible pour des motifs ambiguës ou mal conçus.

## See Also

- Documentation Oracle sur regex: https://docs.oracle.com/javase/tutorial/essential/regex/
- Pour tester vos regex interactivement, utilisez des outils en ligne comme Regex101: https://regex101.com/
- Tutoriel Java sur les regex de W3Schools: https://www.w3schools.com/java/java_regex.asp
