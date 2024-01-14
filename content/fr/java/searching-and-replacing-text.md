---
title:                "Java: Recherche et remplacement de texte"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

La recherche et le remplacement de texte sont des tâches courantes en programmation, nécessaires pour modifier rapidement et efficacement des chaînes de caractères dans un code source. Il est important de maîtriser ces compétences pour gagner du temps lors du développement de projets.

## Comment faire

Pour effectuer une recherche et un remplacement de texte en Java, vous pouvez utiliser la méthode `replace()` de la classe `String`. Elle prend en paramètres la chaîne de caractères à remplacer et la chaîne de remplacement :

```Java
String original = "Bonjour tout le monde!";
String nouvelleChainee = original.replace("Bonjour", "Salut");
System.out.println(nouvelleChainee);
```

L'exemple ci-dessus remplace la chaîne "Bonjour" par "Salut" dans la variable `original`. Le résultat imprimé sera "Salut tout le monde!".

Si vous souhaitez remplacer toutes les occurrences d'une chaîne, vous pouvez utiliser la méthode `replaceAll()` :

```Java
String original = "Ce n'est pas cool";
String nouvelleChainee = original.replaceAll("cool", "génial");
System.out.println(nouvelleChainee);
```

Cela remplacera toutes les occurrences de "cool" par "génial" dans la variable `original` et imprimera le résultat "Ce n'est pas génial".

## Les détails techniques

Lors de l'utilisation de la méthode `replace()` ou `replaceAll()`, il est important de noter que ces méthodes créent une nouvelle chaîne et ne modifient pas la chaîne d'origine. Les chaînes en Java sont immuables, ce qui signifie qu'elles ne peuvent pas être modifiées après leur création.

De plus, ces méthodes utilisent des expressions régulières pour effectuer la recherche de texte. Les expressions régulières sont des motifs de caractères utilisés pour identifier et manipuler des chaînes de caractères. Vous pouvez en apprendre davantage sur les expressions régulières en Java en explorant la classe `Pattern` et la classe `Matcher`.

## Voir aussi
- [Documentation officielle de Java sur la classe String](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- [Tutoriel vidéo sur les expressions régulières en Java](https://www.youtube.com/watch?v=9z9tPtzU5M0&ab_channel=derekbanas)