---
title:    "Arduino: Supprimer les caractères correspondant à un motif"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Pourquoi
Il est parfois nécessaire de supprimer des caractères correspondant à un certain modèle dans un programme Arduino. Cela peut être utile lors de la manipulation de chaînes de caractères ou de données provenant de capteurs.

## Comment faire
Voici un exemple de code pour supprimer les caractères "a" d'une chaîne de caractères en utilisant la fonction `removePattern()` :
```
#include <String.h>

void setup() {
  Serial.begin(9600);
  String myString = "Bonjour, comment ça va?";
  myString.removePattern("a");
  Serial.println(myString);
}

void loop() {

}
```
Le résultat de ce code serait "Bonjour, comment ç v?"

## Plongée en profondeur
Pour supprimer des caractères correspondant à un modèle, il faut utiliser la fonction `removePattern()` de la bibliothèque de chaînes de caractères `String.h`. Cette fonction prend en paramètre le modèle de caractères à supprimer, et peut être utilisée sur une chaîne de caractères existante ou sur une chaîne de caractères nouvellement créée.

Il est également possible d'utiliser des expressions régulières pour supprimer des caractères correspondant à un modèle plus complexe. Pour cela, il faut inclure la bibliothèque `regex.h` et utiliser la fonction `regexReplace()`.

# Voir aussi
- [Documentation officielle de la bibliothèque de chaînes de caractères pour Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/#stringremovepattern)
- [Documentation officielle de la bibliothèque d'expressions régulières pour Arduino](https://www.arduino.cc/en/Reference/RegularExpression)