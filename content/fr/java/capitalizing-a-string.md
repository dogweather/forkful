---
title:                "Capitaliser une chaîne de caractères"
html_title:           "Java: Capitaliser une chaîne de caractères"
simple_title:         "Capitaliser une chaîne de caractères"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Capitaliser une chaîne de caractères en Java signifie mettre la première lettre de chaque mot en majuscule, tout en conservant le reste en minuscules. Les programmeurs le font souvent pour améliorer la lisibilité et la compréhension du texte.

## Comment faire:

Voici un exemple de code pour capitaliser une chaîne de caractères en utilisant la méthode capitalize() de la classe StringUtils.

```Java
String str = "bonjour tout le monde";
System.out.println(StringUtils.capitalize(str));
```

Résultat:

```
Bonjour tout le monde
```

## Plongée en profondeur:

Cette pratique a été introduite dans les langages de programmation pour imiter la façon dont les noms propres sont écrits en anglais. Il existe d'autres méthodes pour capitaliser une string en Java, comme toUpperCase() et substring(). Cependant, elles ne gèrent pas les accents et les caractères spéciaux de la même manière que capitalize().

## Voir aussi:

- [Documentation officielle de la méthode capitalize()](https://docs.oracle.com/javase/7/docs/api/org/apache/commons/lang/StringUtils.html#capitalize(java.lang.String))
- [Guide complet sur la manipulation de chaînes de caractères en Java](https://fr.wikibooks.org/wiki/Programmation_Java/Manipulation_des_cha%C3%AEnes_de_caract%C3%A8res)