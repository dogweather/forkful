---
title:                "Générer des nombres aléatoires"
html_title:           "Elixir: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Générer des nombres aléatoires, c'est produire une séquence de chiffres qui ne peut être prédite logiquement. Les programmeurs le font pour une multitude de raisons, comme simuler des événements naturels ou créer des données d'essai.

## Comment faire :

Voici un exemple de programme Arduino qui génère des nombres aléatoires à l'aide de la fonction `random()`.
```Arduino
void setup() {
  Serial.begin(9600);
  randomSeed(analogRead(0)); //Initialise le générateur de nombres aléatoires
}

void loop() {
  int nombreAleatoire = random(1,100); //Génère un nombre aléatoire entre 1 et 100
  Serial.println(nombreAleatoire);
  delay(1000); //Attends une seconde avant de générer un autre nombre
}
```
Cet exemple génère un nombre aléatoire toutes les secondes et l'envoie à la console série.

## Plongée Profonde :

L'utilisation de `randomSeed(analogRead(0))` est une pratique courante pour initialiser le générateur de nombres aléatoires à un point aléatoire. Historiquement, les premiers ordinateurs généraient des nombres aléatoires en mesurant des phénomènes physiques aléatoires, comme le bruit électrique.

Une alternative à `randomSeed(analogRead(0))` pourrait être `randomSeed(micros())`, qui utilise le temps d'exécution actuel du programme pour initialiser le générateur de nombres aléatoires. Cependant, en utilisant cette méthode, l'état initial peut être plus prévisible si le programme est souvent redémarré.

En termes de mise en œuvre, la fonction `random()` utilise un algorithme appelé "Mersenne Twister", qui est un générateur de nombres pseudo-aléatoires.

## Voir Aussi :

- [La Documentation Officielle Arduino sur la Fonction Random](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Une Article sur Génération des Nombres Aléatoires avec Arduino](https://www.makerguides.com/arduino-random-numbers-tutorial/)
- [Explication de la Méthodologie de RandomSeed](https://www.arduino.cc/reference/en/language/functions/random-numbers/randomseed/)