---
title:                "Concaténation de chaînes de caractères"
date:                  2024-01-20T17:34:51.308108-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concaténation de chaînes de caractères"

category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)

Concaténer des chaînes de caractères, c'est juste les coller bout à bout. Les devs font ça tout le temps pour construire des textes, comme les messages d'erreur ou les infos à afficher.

## How to (Comment faire) :

```java
public class StringConcatExample {
    public static void main(String[] args) {
        // Utilisation de l'opérateur +
        String nom = "Jean";
        String accueil = "Bonjour " + nom + "!";
        System.out.println(accueil); // Affiche: Bonjour Jean!

        // Utilisation de concat()
        String monde = "Monde";
        String salutation = "Salut ".concat(monde).concat("!");
        System.out.println(salutation); // Affiche: Salut Monde!

        // Utilisation de StringBuilder
        StringBuilder sb = new StringBuilder();
        sb.append("Hey, ").append("ça ").append("roule ?");
        System.out.println(sb.toString()); // Affiche: Hey, ça roule ?
    }
}
```

## Deep Dive (Plongée en profondeur) :

Historiquement, concaténer avec l'opérateur `+` était moins performant pour les grandes chaînes ou en boucles. Le compilateur Java transforme le `+` en un `StringBuilder` en coulisse, ce fut une grosse amélioration. Auparavant, l'opérateur pouvait créer beaucoup d'objets inutiles.

StringBuilder et StringBuffer sont des alternatives directes. StringBuilder est plus rapide parce qu'il n'est pas synchronisé, donc parfait pour un seul thread. StringBuffer est thread-safe, utilisez-le quand plusieurs threads touchent à la même chaîne.

## See Also (Voir aussi) :

- [StringBuffer](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuffer.html)
- [StringBuilder](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)
- [Oracle Docs on Operators](https://docs.oracle.com/javase/tutorial/java/nutsandbolts/operators.html)
