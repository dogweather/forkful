---
title:                "Arduino: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi
De nos jours, l'utilisation de l'Arduino et de la programmation informatique est devenue nécessaire dans de nombreux projets. L'extraction de sous-chaînes dans un code Arduino peut sembler sans importance, mais c'est en fait une compétence utile qui peut vous faire gagner du temps et améliorer l'efficacité de vos projets.

## Comment faire
L'extraction de sous-chaînes dans un code Arduino peut être réalisée grâce à certaines fonctions de la bibliothèque standard de l'Arduino. Tout d'abord, vous devez définir une chaîne de caractères dans laquelle vous souhaitez extraire une sous-chaîne, par exemple :
```Arduino
String phrase = "Bonjour à tous !";
```
Ensuite, utilisez la fonction `substring()` pour extraire la sous-chaîne souhaitée en spécifiant l'indice de départ et la longueur de la sous-chaîne :
```Arduino
String sousPhrase = phrase.substring(8,5);
```
Cela extraira la sous-chaîne "à tous" de la chaîne originale. Enfin, utilisez la fonction `print()` pour afficher la sous-chaîne :
```Arduino
Serial.print(sousPhrase);
```
Ce code affichera "à tous" sur le moniteur série. Vous pouvez également stocker la sous-chaîne extraite dans une variable pour une utilisation ultérieure.

## Plongée en profondeur
L'extraction de sous-chaînes peut également être utile lorsque vous travaillez avec des valeurs de capteurs qui contiennent plusieurs informations séparées par des caractères spécifiques ou lorsque vous devez manipuler des chaînes de caractères complexes. Vous pouvez utiliser des méthodes telles que `indexOf()` et `lastIndexOf()` pour trouver l'emplacement d'un caractère spécifique dans une chaîne et ainsi extraire des sous-chaînes plus précises.

## Voir aussi
- [Documentation de la fonction `substring()`](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [Manipulation de chaînes de caractères avec Arduino](https://create.arduino.cc/projecthub/arxivable/manipulating-strings-with-arduino-d0547a)
- [Tutoriel sur les fonctions `indexOf()` et `lastIndexOf()`](https://www.arduino.cc/reference/en/language/functions/advanced-io/lastindexof/)