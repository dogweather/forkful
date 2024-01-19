---
title:                "Mettre une chaîne en majuscules"
html_title:           "Java: Mettre une chaîne en majuscules"
simple_title:         "Mettre une chaîne en majuscules"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi & Pour quoi?

(What & Why?)

La mise en majuscule d'une chaîne consiste à changer la première lettre de chaque mot en majuscule. Les programmeurs le font souvent pour améliorer la lisibilité et l'esthétique d'une interface utilisateur ou pour normaliser les entrées de données.

## Comment faire:

(How to:)

Voici un exemple simple d'utilisation de la méthode `toTitleCase` de l'`java.lang.Character` pour mettre en majuscule une chaîne en Java. 

```Java
String phrase = "c'est un joli jour";
String[] mots = phrase.split("\\s");
StringBuilder nouveauTitre = new StringBuilder();

for(String mot : mots){
    nouveauTitre.append(Character.toTitleCase(mot.charAt(0))).append(mot.substring(1)).append(" ");
}

System.out.println(nouveauTitre.toString().trim());
```

Ce code va produire le résultat suivant :

```
"C'est Un Joli Jour"
```

## Plongée en profondeur :

(Deep Dive)

La précédente méthode `toTitleCase` a été introduite dans Java 8. Avant cela, les programmeurs utilisaient une combinaison de méthodes de la classe `String`. 

Comme alternative à la méthode `toTitleCase`, vous pouvez utiliser `WordUtils.capitalizeFully` de `org.apache.commons.lang3.text` si vous utilisez `Apache Commons`.

D'un point de vue de l'implémentation, `toTitleCase` convertit le caractère en une majuscule Unicode, ce qui est aligné avec le standard Unicode pour le changement de casse.

## Voir aussi :

(See Also)

- Pour plus d'informations sur le changement de casse en Java, consultez la documentation officielle de Java : [java.lang.Character](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/Character.html)
- Pour le traitement de chaînes de caractères avec Apache Commons, voir [org.apache.commons.lang3.text.WordUtils](https://commons.apache.org/proper/commons-lang/javadocs/api-3.1/org/apache/commons/lang3/text/WordUtils.html)
- Pour une compréhension plus approfondie du standard Unicode, visitez le site officiel : [Unicode Standard](https://www.unicode.org/standard/standard.html)