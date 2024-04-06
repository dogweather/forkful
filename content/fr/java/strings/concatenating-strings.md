---
date: 2024-01-20 17:34:51.308108-07:00
description: 'How to (Comment faire) : .'
lastmod: '2024-04-05T21:53:59.132870-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Concat\xE9nation de cha\xEEnes de caract\xE8res"
weight: 3
---

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
