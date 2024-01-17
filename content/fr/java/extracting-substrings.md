---
title:                "Extraction de sous-chaînes"
html_title:           "Java: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?
L'extraction de sous-chaînes est une méthode courante utilisée par les programmeurs Java pour récupérer une partie spécifique d'une chaîne de caractères. Elle est souvent utilisée pour faciliter la manipulation et le traitement des données. Par exemple, si vous voulez trouver le nom d'un utilisateur à partir d'une adresse e-mail, vous pouvez extraire la partie du nom avant le "@" pour obtenir le résultat souhaité. 

## Comment le faire:
```Java
// Déclaration d'une chaîne de caractères
String str = "Bonjour à tous";

// Extraction de la sous-chaîne "tout" à partir de la chaîne donnée
String extractedString = str.substring(11);

// Affichage du résultat
System.out.println(extractedString); // Affiche "tout"
```

## Plongez plus en profondeur:
L'extraction de sous-chaînes existe depuis les premières versions de Java. Elle peut également être réalisée en utilisant des méthodes telles que ```StringTokenizer``` et ```StringBuilder```, cependant, la méthode ```substring``` est considérée comme plus efficace. L'implémentation de cette méthode utilise une fonction interne appelée ```System.arraycopy``` qui copie une partie de la chaîne d'origine dans une nouvelle chaîne, ce qui la rend plus rapide et plus efficace.

## Voir aussi:
- [Documentation officielle de la méthode substring](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int))
- [Autres façons d'extraire une sous-chaîne en Java](http://www.java2s.com/Code/Java/Language-Basics/Extractionsouschaines.htm)