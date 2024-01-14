---
title:    "Java: Extraction de sous-chaînes"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Extraction de sous-chaînes en Java : pourquoi c'est utile ? 

L'extraction de sous-chaînes est une fonctionnalité essentielle en programmation Java. Elle permet d'extraire une partie spécifique d'une chaîne de caractères en fonction de critères définis. Cela peut être utile dans de nombreuses situations, par exemple pour manipuler et traiter des données, ou pour vérifier si une chaîne correspond à un certain format.

## Comment faire

```java
// Exemple d'extraction de sous-chaîne en utilisant la méthode substring()

// Déclaration de la chaîne de caractères initiale
String str = "Bonjour tout le monde";

// Extraction d'une sous-chaîne à partir de l'index 8 inclus jusqu'à la fin
String subStr1 = str.substring(8);

// Extraction d'une sous-chaîne à partir de l'index 8 jusqu'à l'index 14 inclus
String subStr2 = str.substring(8, 15);

// Affichage des résultats
System.out.println(subStr1); // Affiche "tout le monde"
System.out.println(subStr2); // Affiche "tout le"
```

Dans l'exemple ci-dessus, nous utilisons la méthode `substring()` pour extraire des sous-chaînes de différentes longueurs en utilisant des index. Cette méthode peut prendre un ou deux paramètres : le premier paramètre est l'index de départ de l'extraction, et le deuxième paramètre (facultatif) est l'index de fin. Si un seul paramètre est spécifié, la sous-chaîne est extraite à partir de cet index jusqu'à la fin de la chaîne. Si deux paramètres sont spécifiés, la sous-chaîne est extraite à partir de l'index de départ jusqu'à l'index de fin inclus.

```java
// Exemple d'extraction de sous-chaîne en utilisant la méthode split()

// Déclaration de la chaîne de caractères initiale
String str = "Le saviez-vous ? Java est un langage de programmation objet.";

// Extraction d'une sous-chaîne à partir du premier espace rencontré
String subStr1 = str.split(" ")[0];

// Extraction d'une sous-chaîne à partir du troisième mot
String subStr2 = str.split(" ")[2];

// Affichage des résultats
System.out.println(subStr1); // Affiche "Le"
System.out.println(subStr2); // Affiche "Java"
```

Dans cet exemple, nous utilisons la méthode `split()` pour séparer la chaîne en plusieurs sous-chaînes en utilisant un délimiteur (ici l'espace). Une fois la chaîne séparée, nous pouvons accéder à chaque sous-chaîne en utilisant son index dans le tableau de résultats.

## Plongez plus profondément

L'extraction de sous-chaînes peut être un sujet complexe, en particulier lorsqu'il s'agit de manipuler des chaînes en utilisant des expressions régulières. Pour plus d'informations sur les différentes méthodes d'extraction de sous-chaînes en Java, vous pouvez consulter la documentation officielle de Java ou des ressources en ligne telles que [ce tutoriel](https://www.baeldung.com/java-substring) (en anglais).

## Voir aussi

- [Documentation officielle de Java sur la classe String](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Tutoriel sur l'utilisation des expressions régulières en Java](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)