---
title:                "Démarrer un nouveau projet"
html_title:           "Elm: Démarrer un nouveau projet"
simple_title:         "Démarrer un nouveau projet"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Démarrer un nouveau projet est l'acte de créer une base pour une nouvelle application à partir de zéro. Les programmeurs le font pour concrétiser leurs idées, résoudre un problème, ou simplement pour apprendre.

## Comment faire :

Le code se trouve dans des blocs ```Arduino ... ```, comme celui-ci:
```Arduino
void setup() {
  pinMode(LED_BUILTIN, OUTPUT);
}

void loop() {
  digitalWrite(LED_BUILTIN, HIGH);
  delay(1000);
  digitalWrite(LED_BUILTIN, LOW);
  delay(1000);
}
```
C'est un exemple de clignotement d'une LED intégrée. Si l'exécution est réussie, la LED intégrée à votre carte Arduino clignotera toutes les secondes.

## Plongée profonde :

L'Arduino est né en 2005 dans une école de design italienne, dans le but de rendre la programmation abordable et accessible à tous. C'est une excellente façon de commencer avec la programmation en C++.

Une alternative à Arduino serait Raspberry Pi : plus puissant, mais aussi plus complexe. L'Arduino est souvent le premier choix pour les débutants en raison de sa simplicité.

Lors de la création d'un nouveau projet Arduino, il est essentiel de comprendre les étapes de base : définir les entrées/sorties (via `pinMode()`), écrire la logique de votre programme (dans `loop()`) et initialiser les paramètres (dans `setup()`).

## Voir aussi :

Pour approfondir votre apprentissage de la programmation Arduino, voici quelques ressources utiles (en anglais) :

1. Le site officiel d'Arduino : [Arduino Official Website](https://www.arduino.cc/)
2. Pour une référence rapide à la syntaxe : [Arduino Language Reference](https://www.arduino.cc/reference/en/)
3. Pour des exemples de projets et tutoriels : [Arduino Project Hub](https://create.arduino.cc/projecthub).