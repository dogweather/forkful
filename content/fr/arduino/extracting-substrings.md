---
title:                "Extraction de sous-chaînes"
html_title:           "Arduino: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

L'extraction des sous-chaînes consiste à prendre une partie spécifique d'une chaîne de caractères plus longue. Les programmeurs le font souvent pour extraire des informations précises d'une chaîne plus grande ou pour faciliter le traitement de données.

## Comment :

Voici deux exemples simples d'extraction de sous-chaînes en utilisant la fonction ```substring()``` de l'Arduino. Cette fonction prend deux paramètres : l'index de départ et la longueur de la sous-chaîne.

Exemple 1: Extraction à partir de l'index 3 jusqu'à la fin de la chaîne
```Arduino
String str = "Bonjour tout le monde!";
String substr = str.substring(3);
Serial.println(substr);
```
Output: `jour tout le monde!`

Exemple 2: Extraction d'une sous-chaîne de 5 caractères à partir de l'index 0
```Arduino
String str = "123456789";
String substr = str.substring(0, 5);
Serial.println(substr);
```
Output: `12345`

Notez que la numérotation des index commence à 0 et non à 1.

## Plongée en profondeur :

L'extraction de sous-chaînes est une fonctionnalité courante dans de nombreux langages de programmation. Elle est souvent utilisée dans le traitement de données, l'analyse de texte et la manipulation de chaînes complexes.

Alternatives : En plus de la fonction ```substring()```, Arduino propose également la fonction ```subString()``` qui fonctionne de la même manière, mais n'utilise pas de mémoire dynamique. Cela permet d'économiser de la mémoire si vous travaillez avec de grandes chaînes de caractères.

Détails d'implémentation : La fonction ```substring()``` utilise la méthode ```substring()``` de la classe String de l'Arduino pour extraire les sous-chaînes. Elle est basée sur la bibliothèque standard C++ et peut donc être utilisée dans vos projets sans aucun problème.

## Voir aussi :

- [Documentation officielle de la classe String de l'Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Guide de référence rapide pour l'Arduino](https://www.arduino.cc/reference/en/)