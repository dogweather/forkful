---
date: 2024-01-26 01:16:36.374873-07:00
description: 'Comment faire : Disons que vous avez une fonction sur votre Arduino
  qui fait beaucoup trop, comme ceci .'
lastmod: '2024-03-13T22:44:58.121853-06:00'
model: gpt-4-0125-preview
summary: Disons que vous avez une fonction sur votre Arduino qui fait beaucoup trop,
  comme ceci.
title: Refactoring
weight: 19
---

## Comment faire :
Disons que vous avez une fonction sur votre Arduino qui fait beaucoup trop, comme ceci :

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  // Une fonction qui fait trop
  handleEverything();
}

void handleEverything() {
  // Lire les données du capteur
  int sensorValue = analogRead(A0);
  // Traiter les données du capteur
  sensorValue = map(sensorValue, 0, 1023, 0, 255);
  // Imprimer les données du capteur
  Serial.println(sensorValue);
  delay(500);
}
```

Le refactoriser pourrait ressembler à diviser `handleEverything()` en fonctions plus petites et plus ciblées :

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  int sensorValue = readSensorData();
  int processedValue = processSensorData(sensorValue);
  printData(processedValue);
  delay(500);
}

int readSensorData() {
  return analogRead(A0);
}

int processSensorData(int sensorValue) {
  return map(sensorValue, 0, 1023, 0, 255);
}

void printData(int data) {
  Serial.println(data);
}
```

Après le refactoring, la fonction `loop()` est plus lisible et chaque tâche est gérée par une fonction dédiée, rendant le code plus facile à gérer.

## Approfondissement
Historiquement, le refactoring est devenu populaire avec l'ascension des méthodologies Agile et du Développement Piloté par les Tests (TDD), qui comptent sur l'amélioration constante du code pour s'adapter aux exigences changeantes. Il existe divers outils et stratégies pour le refactoring — comme la technique "Extract Method" que nous avons utilisée dans notre exemple Arduino. Ceci est essentiel lorsque vous passez d'un prototype rapide à un projet stable, où la lisibilité et la maintenance du code deviennent cruciales.

Lors du refactoring, il est important d'avoir un bon ensemble de tests en place pour garantir que les changements n'ont introduit aucun bug. Dans le monde Arduino, le test automatique n'est pas toujours simple en raison des dépendances matérielles, mais vous pouvez toujours utiliser des tests unitaires pour les parties de logique pure ou utiliser des simulateurs.

Les alternatives au refactoring manuel incluent l'utilisation d'outils de refactoring dédiés, qui automatisent l'identification des odeurs de code et suggèrent des changements. Cependant, ces outils manquent souvent de nuances pour le code de microcontrôleur et pourraient ne pas être disponibles dans l'environnement de développement Arduino.

En fin de compte, le refactoring est un art qui équilibre l'amélioration de la structure interne du code contre le risque d'introduire des défauts. Cela vous oblige à penser aux détails de l'implémentation comme l'usage de la mémoire et le temps processeur, surtout en raison de la nature des microcontrôleurs à ressources limitées.

## Voir également
Vous pouvez approfondir le refactoring avec le livre séminal de Martin Fowler *Refactoring : améliorer la conception du code existant*. Pour un regard plus attentif sur les pratiques spécifiques à Arduino, consultez les forums et communautés de développement Arduino :

- [Forum Arduino - Questions de programmation](https://forum.arduino.cc/index.php?board=4.0)
- [Refactoring Guru](https://refactoring.guru/refactoring)

Rappelez-vous, l'objectif est un code propre et compréhensible pour lequel le vous du futur, et les autres, vous remercieront. Continuez à hacker, et gardez-le propre !
