---
title:                "Concaténation de chaînes"
html_title:           "C: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

La Concaténation de chaînes est le processus de fusion de deux chaînes de caractères séparées en une seule. Les programmeurs le font quand ils veulent unir plusieurs morceaux d'information textuelle en une seule chaîne.

## Comment faire:

Voici une démonstration de la concaténation de chaînes en Arduino. Elle implique l'utilisation de la fonction strcat(). Pour saisir la méthode, réfère à cet exemple :

```Arduino 
  char texte1[50] = "Salut ";
  char texte2[50] = "Arduino!";

  strcat(texte1, texte2);

  Serial.begin(9600);
  Serial.println(texte1);
```
Quand vous exécutez ce code, vous verrez la sortie :

```Arduino
  Salut Arduino!
```

## Immersion Profonde :
 
La Concaténation de chaînes a été utilisée depuis les débuts de la programmation où la gestion efficace de la mémoire était essentielle. En Arduino, vous pouvez également utiliser la méthode `+=` pour concaténer, bien que cela puisse parfois conduire à une utilisation inefficace de la mémoire. 

```Arduino
String salut = "Salut ";
salut += "Arduino!";

Serial.begin(9600);
Serial.println(salut);
```

La fonction Arduino append(), qui ajoute un caractère ou une chaîne de caractères à la fin d'une autre chaîne, est également une alternative pour la concaténation de chaînes.

## Voir Aussi :

1. [Documentation Arduino](https://www.arduino.cc/reference/fr/language/variables/data-types/string/functions/concat/): Détails sur les string en Arduino
3. [Wikipedia Concatenation](https://fr.wikipedia.org/wiki/Concat%C3%A9nation): L'article Wikipedia sur l'histoire et l'application générale de la concaténation de chaînes.