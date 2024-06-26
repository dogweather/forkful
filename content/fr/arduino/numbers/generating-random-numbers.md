---
date: 2024-01-27 20:32:41.674315-07:00
description: "Comment faire : Arduino fournit des fonctions simples pour g\xE9n\xE9\
  rer des nombres al\xE9atoires : `randomSeed()` et `random()`. Pour commencer, initialisez\
  \ le\u2026"
lastmod: '2024-04-05T22:38:58.601920-06:00'
model: gpt-4-0125-preview
summary: "Arduino fournit des fonctions simples pour g\xE9n\xE9rer des nombres al\xE9\
  atoires ."
title: "G\xE9n\xE9ration de nombres al\xE9atoires"
weight: 12
---

## Comment faire :
Arduino fournit des fonctions simples pour générer des nombres aléatoires : `randomSeed()` et `random()`. Pour commencer, initialisez le générateur de nombres aléatoires pour garantir différentes séquences de nombres à chaque exécution de votre programme. Une approche souvent utilisée consiste à initialiser avec une lecture analogique à partir d'une broche non connectée.

```Arduino
void setup() {
  Serial.begin(9600);
  // Initialiser le générateur aléatoire
  randomSeed(analogRead(0));
}

void loop() {
  // Générer un nombre aléatoire entre 0 et 99
  int randomNumber = random(100);
  Serial.println(randomNumber);
  delay(1000); // Délai d'une seconde pour la lisibilité de la sortie
}
```

Le programme ci-dessus initialise le générateur de nombres aléatoires dans la fonction `setup()` et génère un nouveau nombre entre 0 et 99 à chaque itération de la boucle, affichant le nombre dans le moniteur série.

Exemple de sortie :
```
42
17
93
...
```

## Exploration approfondie
La fonction `random()` d'Arduino utilise en arrière-plan un générateur de nombres pseudo-aléatoires (PRNG), qui suit une séquence déterministe mais qui semble statistiquement aléatoire. La valeur initiale, ou graine, de la séquence influence fortement son imprévisibilité, d'où l'utilisation courante de `randomSeed()` avec une entrée quelque peu aléatoire comme point de départ. Il est important de noter que l'aléatoire généré par Arduino est suffisant pour la plupart des projets de hobbyistes mais peut ne pas répondre aux critères des applications à haute sécurité en raison de sa prévisibilité dans le temps. Pour les fins cryptographiques, il est conseillé de se pencher sur des algorithmes plus sophistiqués et des générateurs de nombres aléatoires matériels (HRNGs), qui peuvent fournir une véritable aléatoire en utilisant des processus physiques.
