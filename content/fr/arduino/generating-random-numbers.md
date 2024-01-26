---
title:                "Génération de nombres aléatoires"
date:                  2024-01-20T17:48:36.277977-07:00
model:                 gpt-4-1106-preview
simple_title:         "Génération de nombres aléatoires"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Générer des nombres aléatoires, c'est comme tirer au sort sans chapeau. Les programmeurs s'en servent pour tout : des jeux à la simulation scientifique.

## How to:
```Arduino
void setup() {
  // Initialize the serial communication
  Serial.begin(9600);
  // Seed the random number generator
  randomSeed(analogRead(0));
}

void loop() {
  // Generate a random number between 1 and 100
  int randomNumber = random(1, 101);
  // Print the random number
  Serial.println(randomNumber);
  // Wait for a second
  delay(1000);
}
```
*Résultat sample:* `42`, `17`, `93`, chaque seconde un nouveau numéro.

## Deep Dive
La fonction `randomSeed()` initialise le générateur de nombres aléatoires d'Arduino—sans ça, la "séquence aléatoire" serait toujours la même. Historiquement, on a souvent utilisé des perturbations comme le bruit et les tensions pour créer de l'aléatoire. Avant `randomSeed()`, on pourrait utiliser `noise` ou capteurs. Mais Arduino simplifie tout avec sa bibliothèque.

Un détail croustillant : ces nombres ne sont pas vraiment aléatoires, mais pseudo-aléatoires. Pourquoi? Parce qu'ils sont calculés via des algorithmes déterministes. Ça suffit pour beaucoup d'applications, mais pas toutes. Pour de l'aléatoire "vraiment" aléatoire, on se tournerait vers des phénomènes physiques imprévisibles.

## See Also
- Arduino Reference for `random()`: https://www.arduino.cc/reference/en/language/functions/random-numbers/random/
- Arduino Reference for `randomSeed()`: https://www.arduino.cc/reference/en/language/functions/random-numbers/randomseed/
- "Pseudo-random number generation" on Wikipedia: https://en.wikipedia.org/wiki/Pseudorandom_number_generator
