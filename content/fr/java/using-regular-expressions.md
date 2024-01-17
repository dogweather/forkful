---
title:                "Utiliser des expressions régulières"
html_title:           "Java: Utiliser des expressions régulières"
simple_title:         "Utiliser des expressions régulières"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

Les expressions régulières (ou regexp) sont un outil de programmation puissant utilisé pour rechercher et manipuler des chaînes de caractères dans un texte. Les programmeurs les utilisent pour effectuer des opérations telles que la validation d'entrées utilisateur, la recherche de motifs dans un fichier ou la modification de données. Les expressions régulières sont un moyen efficace de traiter de grandes quantités de données avec une seule instruction.

## Comment faire:

Voici un exemple de code Java pour rechercher et afficher les noms avec un préfixe "M." dans une liste de noms:

```java
import java.util.regex.*;

public class RegularExpressions {

    public static void main(String[] args) {

        // Crée un motif pour correspondre au préfixe "M."
        Pattern pattern = Pattern.compile("M\\..*");

        // Initialise une liste de noms
        String[] names = {"M. Smith", "John", "M. Johnson", "Samantha"};

        // Boucle à travers les noms et vérifie le motif
        for (String name : names) {
            Matcher matcher = pattern.matcher(name);
            if (matcher.matches()) {
                System.out.println(name);
            }
        }
    }
}
```

Output:
```
M. Smith
M. Johnson
```

## Plongée en profondeur:

Les expressions régulières existent depuis les années 1950 et ont été inventées par le mathématicien Stephen Cole Kleene. Bien qu'elles soient largement utilisées dans de nombreux langages de programmation, il existe des alternatives telles que les méthodes de manipulation de chaînes de caractères intégrées dans certains langages. Les expressions régulières peuvent être complexes à comprendre et à utiliser au début, mais une fois maîtrisées, elles peuvent être très utiles pour effectuer des tâches répétitives.

## Voir aussi:

- [Documentation officielle de Java sur les expressions régulières](https://docs.oracle.com/javase/8/docs/api/java/util/regex/package-summary.html)
- [Tutoriel complet sur les expressions régulières en Java](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)