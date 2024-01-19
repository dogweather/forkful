---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "C: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

La suppression des caractères correspondant à un modèle est un concept où des caractères prédéfinis dans une chaîne de caractères sont supprimés. Les programmeurs le font souvent pour nettoyer les données et faciliter le traitement des informations.

## Comment Faire: 

Nous allons utiliser la fonction `remove()` de la bibliothèque String pour supprimer les caractères correspondant à un motif dans Arduino. Voici un exemple :

```Arduino
String myString = "Renard Couleur ~~Rouge~~";
myString.remove(13, 11);  // supprime le motif ~~Rouge~~

void setup() 
{
  Serial.begin(9600);
}

void loop() 
{
  Serial.println(myString); // affiche "Renard Couleur "
  delay(1000);
}
```

Lorsque vous exécutez ce code, vous verrez que le motif "~~Rouge~~" a été supprimé de la chaîne. La sortie sera "Renard Couleur ".

## Zoom :

L'Arduino ne possédait pas à l'origine la possibilité de manipuler des chaînes de caractères. Cependant, avec l'introduction de la bibliothèque `String` dans l'Arduino version 0019, les développeurs ont désormais accès à une série de fonctions de manipulation de chaînes, notamment la suppression de caractères.

En termes d'alternatives, vous pouvez également utiliser des tableaux de caractères et la bibliothèque `cstring` pour réaliser des manipulations similaires. Cependant, la bibliothèque `String` facilite grandement ce processus.

En termes de mise en œuvre, lors de l'appel de la fonction `remove()`, la chaîne est d'abord scannée pour trouver le premier caractère du motif. Une fois ce caractère trouvé, les caractères suivants sont comparés pour vérifier si le motif complet est correspondant.

## Voir Aussi :

Pour plus de détails sur la manipulation des chaînes de caractères dans Arduino, consultez ces liens :

1. [Documentation Arduino String](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
2. [Arduino String Tutorial](https://www.tutorialspoint.com/arduino/arduino_strings.htm)
3. [Détails sur la mise en œuvre de la fonction String::remove()](https://github.com/arduino/ArduinoCore-API/blob/master/api/String.cpp)