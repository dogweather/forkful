---
date: 2024-01-26 01:08:47.221061-07:00
description: "Organiser son code en fonctions signifie le d\xE9couper en blocs r\xE9\
  utilisables, chaque bloc ex\xE9cutant une t\xE2che sp\xE9cifique. Les programmeurs\
  \ le font pour\u2026"
lastmod: '2024-03-13T22:44:58.117761-06:00'
model: gpt-4-1106-preview
summary: "Organiser son code en fonctions signifie le d\xE9couper en blocs r\xE9utilisables,\
  \ chaque bloc ex\xE9cutant une t\xE2che sp\xE9cifique."
title: Organisation du code en fonctions
weight: 18
---

## Quoi et pourquoi ?
Organiser son code en fonctions signifie le découper en blocs réutilisables, chaque bloc exécutant une tâche spécifique. Les programmeurs le font pour rendre le code plus lisible, faciliter le débogage et la réutilisation. C'est comme trier des Legos dans des bacs – cela vous évite de fouiller dans un tas chaotique à chaque fois que vous voulez construire quelque chose.

## Comment faire :
Imaginez que vous voulez faire clignoter une LED. Sans fonctions, votre `loop` est un gâchis enchevêtré. Avec des fonctions, il est ordonné. Voici comment :

```Arduino
const int LED_PIN = 13;

void setup() {
  pinMode(LED_PIN, SORTIE);
}

void loop() {
  blinkLED(500); // Fait clignoter la LED toutes les 500ms
}

// Fonction pour faire clignoter une LED
void blinkLED(int delayTime) {
  digitalWrite(LED_PIN, HAUT);
  delay(delayTime);
  digitalWrite(LED_PIN, BAS);
  delay(delayTime);
}
```

Résultat de l'exemple : Votre LED clignote joyeusement, et le but du code est immédiatement évident.

## Plongée en profondeur
Avant les fonctions, la programmation était un voyage routier linéaire ; vous voyiez chaque nid-de-poule du début à la fin. Avec les fonctions, c'est plus comme sauter des vols - vous passez aux parties importantes. Historiquement, les sous-routines (fonctions primitives) ont été une révolution dans la programmation, permettant aux codeurs d'éviter de se répéter – c'est le principe DRY, Don’t Repeat Yourself (Ne vous répétez pas). Les alternatives aux fonctions pourraient inclure des macros ou l'utilisation de classes pour la programmation orientée objet (POO). Le fin mot de l'histoire ? Lorsque vous définissez une fonction, vous fournissez au compilateur un plan pour exécuter une tâche. Avec Arduino, vous définissez souvent des fonctions void qui agissent comme de simples commandes pour un microcontrôleur, mais les fonctions peuvent aussi retourner des valeurs, les rendant ainsi plus polyvalentes.

## Voir aussi
Pour en savoir plus sur les fonctions, parcourez ces ressources :

- Référence officielle des fonctions d'Arduino : https://www.arduino.cc/reference/en/language/functions/
- En savoir plus sur le principe DRY : https://fr.wikipedia.org/wiki/Ne_vous_répétez_pas
- Une remise à niveau sur l'histoire des sous-routines : https://fr.wikipedia.org/wiki/Sous-routine

(Note: Some links redirect to English pages as the equivalent French page might not exist.)
