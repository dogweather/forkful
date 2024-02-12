---
title:                "Mettre en majuscule une chaîne"
aliases:
- /fr/arduino/capitalizing-a-string.md
date:                  2024-02-03T19:04:59.300172-07:00
model:                 gpt-4-0125-preview
simple_title:         "Mettre en majuscule une chaîne"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Mettre en majuscule une chaîne de caractères consiste à convertir le premier caractère de chaque mot en majuscule, tout en s'assurant que les autres restent en minuscule. Cette opération est courante dans la mise en forme des données et la normalisation des entrées utilisateur pour maintenir la cohérence et améliorer la lisibilité.

## Comment faire :
Arduino, principalement connu pour son interaction avec le matériel, inclut également des capacités de manipulation de chaînes de caractères basiques via son objet `String`. Cependant, il n'a pas de fonctions `capitalize` directe comme on peut le voir dans les langages de plus haut niveau. Ainsi, nous implémentons la capitalisation en itérant sur une chaîne de caractères et en appliquant des transformations de casse.

Voici un exemple simple sans utiliser de bibliothèques tierces :

```cpp
String capitalizeString(String input) {
  if (input.length() == 0) {
    return ""; // Retourner une chaîne vide si l'entrée est vide
  }
  input.toLowerCase(); // Convertir d'abord la chaîne entière en minuscules
  input.setCharAt(0, input.charAt(0) - 32); // Mettre en majuscule le premier caractère
  
  // Mettre en majuscule les lettres qui suivent un espace
  for (int i = 1; i < input.length(); i++) {
    if (input.charAt(i - 1) == ' ') {
      input.setCharAt(i, input.charAt(i) - 32);
    }
  }
  return input;
}

void setup() {
  Serial.begin(9600);
  String testStr = "hello arduino world";
  String capitalizedStr = capitalizeString(testStr);
  Serial.println(capitalizedStr); // Sortie : "Hello Arduino World"
}

void loop() {
  // Boucle vide
}
```

Ce fragment de code définit une fonction `capitalizeString` qui convertit d'abord la chaîne entière en minuscules pour standardiser sa casse. Il met ensuite en majuscule le premier caractère et tout caractère qui suit un espace, mettant effectivement en majuscule chaque mot dans la chaîne d'entrée. Notez que cette mise en œuvre rudimentaire suppose un encodage de caractères ASCII et peut nécessiter des ajustements pour un support complet de l'Unicode.

Actuellement, il n'y a pas de bibliothèques tierces largement adoptées spécifiquement pour la manipulation de chaînes dans l'écosystème Arduino, principalement en raison de son accent sur l'interaction matérielle et l'efficacité. Cependant, l'exemple fourni est une manière directe d'atteindre la capitalisation de chaînes au sein de l'environnement de programmation Arduino.
