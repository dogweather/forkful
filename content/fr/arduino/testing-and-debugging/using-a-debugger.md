---
date: 2024-01-26 03:47:20.321423-07:00
description: "Comment faire : Avec l'IDE Arduino, vous pouvez utiliser des impressions\
  \ s\xE9rie pour d\xE9boguer, mais c'est un peu comme utiliser une lampe de poche\
  \ pour\u2026"
lastmod: '2024-03-13T22:44:58.115979-06:00'
model: gpt-4-0125-preview
summary: "Avec l'IDE Arduino, vous pouvez utiliser des impressions s\xE9rie pour d\xE9\
  boguer, mais c'est un peu comme utiliser une lampe de poche pour explorer une grotte."
title: "Utilisation d'un d\xE9bogueur"
weight: 35
---

## Comment faire :
Avec l'IDE Arduino, vous pouvez utiliser des impressions série pour déboguer, mais c'est un peu comme utiliser une lampe de poche pour explorer une grotte. Pour un véritable débogage, vous pourriez vouloir élever votre niveau avec quelque chose comme le débogueur Atmel-ICE qui s'intègre à l'environnement Arduino. Voici un avant-goût du pseudo-débogage utilisant Serial :

```Arduino
void setup() {
  Serial.begin(9600);
}
void loop() {
  int sensorValue = analogRead(A0);
  Serial.print("Valeur du Capteur : ");
  Serial.println(sensorValue);
  // Imaginez que vous attendez 512 ici, mais obtenez 0.
  // Il est temps d'inspecter la connexion du capteur
  delay(1000); // Attendre une seconde avant de lire à nouveau
}
```
Exécutez ceci avec le moniteur série ouvert, et vous verrez ce que votre capteur crache en temps réel.

## Plongée profonde
Avant les débogueurs, c'était le monde des instructions d'impression – vous ne pouviez deviner ce qui se passait qu'en imprimant tout. Le débogage avec des impressions est toujours courant, en particulier dans des environnements simples ou sur du matériel contraint comme l'Arduino.

Les alternatives aux émulateurs en circuit comme Atmel-ICE incluent des outils de débogage logiciel comme `avr-gdb`. Vous pouvez le coupler avec `avarice` pour créer un pont entre GDB et votre matériel, ce qui est super pratique pour un débogage plus avancé directement sur la puce.

En utilisant un débogueur, vous pouvez définir des points d'arrêt pour arrêter l'exécution à certains points. Vous pouvez passer à travers votre code ligne par ligne, inspecter la mémoire, les registres et les variables. Cela vous permet de cibler les problèmes au lieu de tâtonner dans le noir. Lors de l'implémentation d'un débogueur, assurez-vous que votre environnement est correctement configuré - des versions incompatibles ou des outils mal configurés peuvent conduire à la frustration.

## Voir également
Prêt à aller plus loin ? Plongez dans ceux-ci :
- Le guide de débogage Arduino sur [Arduino Debugging](https://www.arduino.cc/en/Guide/Environment#toc7)
- Le manuel de référence AVR Libc pour configurer avr-gdb : [Page d'accueil AVR Libc](http://www.nongnu.org/avr-libc/)
