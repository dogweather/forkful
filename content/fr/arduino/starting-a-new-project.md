---
title:    "Arduino: Commencer un nouveau projet"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous avez récemment acheté un Arduino et vous vous demandez comment commencer à programmer avec cet outil ? Vous êtes au bon endroit ! Dans cet article, je vais vous expliquer pourquoi apprendre à programmer avec Arduino peut être une activité passionnante et utile.

## Comment faire

Tout d'abord, il est important de comprendre les bases de la programmation et de l'électronique. Arduino utilise le langage de programmation C/C++ et nécessite également une bonne compréhension des circuits électroniques.

Pour commencer, vous aurez besoin de télécharger et d'installer le logiciel Arduino sur votre ordinateur. Ensuite, vous pouvez commencer à créer votre premier projet en utilisant des composants électroniques tels que des LED, des résistances et des capteurs.

Voici un exemple de code pour allumer une LED avec Arduino :
```Arduino
void setup() {
  pinMode(LED_BUILTIN, OUTPUT);  // Définit la broche LED comme une sortie
}

void loop() {
  digitalWrite(LED_BUILTIN, HIGH); // Allume la LED
  delay(1000); // Attend 1 seconde
  digitalWrite(LED_BUILTIN, LOW); // Éteint la LED
  delay(1000); // Attend 1 seconde
}
```

Vous pouvez également ajouter des conditions et des boucles pour créer des projets plus complexes. Le site officiel d'Arduino propose une documentation complète ainsi que des exemples de code pour vous aider à démarrer.

## Plongée en profondeur

En plus de l'aspect pratique, apprendre à programmer avec Arduino peut également être très enrichissant sur le plan éducatif. Cela vous donnera une meilleure compréhension de la technologie et des systèmes électroniques qui nous entourent.

Vous pouvez également créer des projets qui peuvent être utiles dans la vie quotidienne, tels qu'un système de surveillance à domicile ou un thermostat intelligent. La seule limite est votre imagination !

## Voir aussi

Pour continuer à apprendre et à explorer les possibilités infinies d'Arduino, voici quelques liens utiles :

- [Site officiel d'Arduino](https://www.arduino.cc/) : documentation, exemples de projets et forums communautaires
- [Adafruit](https://learn.adafruit.com/category/learn-arduino) : tutoriels et projets intéressants pour débutants et experts
- [Hackster](https://www.hackster.io/arduino/projects) : une communauté de makers partageant leurs projets et leurs connaissances sur Arduino

Maintenant que vous avez les bases, il ne vous reste plus qu'à vous lancer et à créer vos propres projets avec Arduino !