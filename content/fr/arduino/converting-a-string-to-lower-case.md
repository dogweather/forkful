---
title:                "Convertir une chaîne en minuscules"
html_title:           "Arduino: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Convertir une chaîne de caractères en minuscules est une fonctionnalité importante pour les programmeurs. Cela permet de standardiser les données et de faciliter la manipulation de chaînes de caractères en les rendant toutes en minuscules. Cela peut être utile pour la comparaison de chaînes ou pour l'affichage de données dans un format uniforme.

## Comment faire :

La conversion d'une chaîne de caractères en minuscules est très simple à réaliser en utilisant l'Arduino. Il suffit d'utiliser la fonction ```toLowerCase()```, qui prend en paramètre la chaîne de caractères que l'on souhaite convertir en minuscules. Voici un exemple de code :

```
String myString = "Hello World!";
String lowerString = myString.toLowerCase();
Serial.println(lowerString); // Output: hello world!
```

## En profondeur :

La fonction ```toLowerCase()``` est disponible pour les chaînes de caractères depuis la version 1.0. Cette fonction utilise la table ASCII pour convertir les caractères en minuscules. Il est important de noter que cette fonction ne modifie pas la chaîne originale, mais renvoie une nouvelle chaîne en minuscules.

Une alternative à l'utilisation de la fonction ```toLowerCase()``` est d'utiliser une boucle pour parcourir chaque caractère de la chaîne et de le convertir en minuscule en utilisant la table ASCII. Cependant, cela serait plus laborieux et moins efficace.

En termes d'implémentation, la fonction ```toLowerCase()``` utilise la fonction ```tolower()``` du langage C, qui est également basée sur la table ASCII.

## À voir aussi :

Pour en savoir plus sur l'utilisation des chaînes de caractères en Arduino, vous pouvez consulter la documentation officielle ici : https://www.arduino.cc/reference/en/language/variables/data-types/string/

Pour voir l'exemple de code complet utilisé dans cet article, vous pouvez le trouver sur GitHub ici : https://github.com/ArduinoMax/convert-string-to-lowercase