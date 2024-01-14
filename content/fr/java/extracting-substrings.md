---
title:                "Java: Extraction de sous-chaînes"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi
Extractions de sous-chaînes dans Java peuvent être très utiles lorsque vous avez une chaîne de caractères et que vous voulez en extraire une partie spécifique pour une utilisation ultérieure. Cela peut être particulièrement utile lorsque vous travaillez avec des données de texte et que vous avez besoin d'extraire des informations précises.

## Comment faire
```Java
// Création d'une chaîne de caractères
String chaine = "Bienvenue au blog de programmation en Java !";

// Extraction d'une sous-chaîne à partir de l'index 4
String sousChaine = chaine.substring(4);
System.out.println(sousChaine);

// Sortie: venue au blog de programmation en Java !

// Extraction d'une sous-chaîne entre les index 11 et 22 inclus
String sousChaine2 = chaine.substring(11, 22);
System.out.println(sousChaine2);

// Sortie: blog de prog
```

Avec la méthode `substring` en Java, vous pouvez également extraire une sous-chaîne en utilisant des indices négatifs, ce qui le rend encore plus flexible. Par exemple, vous pouvez utiliser `-5` pour extraire les 5 derniers caractères de la chaîne.

## Deep Dive
La méthode `substring` en Java prend en compte l'index de début et l'index de fin de la sous-chaîne à extraire. L'index de début est inclus dans la sous-chaîne tandis que l'index de fin ne l'est pas. Cela signifie que la sous-chaîne extrait commence à l'index de début et va jusqu'à l'index de fin - 1.

De plus, avec la méthode `substring`, vous pouvez également spécifier uniquement l'index de début et la sous-chaîne sera extraite jusqu'à la fin de la chaîne d'origine.

## Voir aussi
- [Méthode `substring` dans la documentation Java](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int-)
- [Tutoriel sur les chaînes de caractères en Java](https://www.javatpoint.com/java-string) 
- [Guide des expressions régulières en Java](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)