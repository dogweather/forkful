---
title:                "Arduino: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi
Supprimer des caractères qui correspondent à un certain motif peut être utile pour filtrer des données ou pour simplifier la manipulation d'une chaîne de caractères. Cela peut également être utile dans des projets où des données spécifiques doivent être collectées ou affichées.

## Comment faire
Pour supprimer des caractères correspondant à un motif, nous allons utiliser la fonction `replace()` de la bibliothèque `String` d'Arduino. Cette fonction prend deux paramètres : le motif à rechercher et la valeur par laquelle il doit être remplacé. Voici un exemple de code :

```Arduino
#include <String.h>

void setup() {
  Serial.begin(9600); // Initialise la communication série
  String message = "Bonjour Arduino !"; // Chaîne de caractères à traiter
  Serial.println(message); // Affiche la chaîne de caractères originale
  
  // Remplace toutes les lettres 'o' par des 'a'
  message.replace('o', 'a');
  
  Serial.println(message); // Affiche la chaîne de caractères modifiée
}

void loop() {

}
```

Output:
```
Bonjour Arduino !
Banjaar Arduina !
```

## Plongée en profondeur
La fonction `replace()` est très utile pour supprimer des caractères correspondant à un motif, mais elle a ses limites. Elle ne peut remplacer qu'un caractère à la fois et ne prend pas en compte les majuscules et les minuscules. Pour une plus grande flexibilité, nous pouvons utiliser d'autres fonctions de la bibliothèque `String` telles que `remove()` ou `replaceAll()`. Il est également important de noter que ces fonctions peuvent consommer beaucoup de mémoire, il est donc préférable de les utiliser avec parcimonie dans vos projets Arduino.

## Voir aussi
- [Documentation officielle d'Arduino sur la fonction `replace()`](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Tutoriel de programmation pour débutants sur le traitement de chaînes de caractères avec Arduino](https://www.circuitbasics.com/basics-arduino-string-processing/)
- [Forum de la communauté Arduino pour poser vos questions et partager vos connaissances](https://forum.arduino.cc/)