---
date: 2024-01-20 17:51:50.012697-07:00
description: "Imprimer un d\xE9bogage, c'est \xE9crire dans la console pour suivre\
  \ ce que fait votre programme. On le fait pour rep\xE9rer les bugs et comprendre\
  \ le flux\u2026"
lastmod: '2024-03-13T22:44:58.113001-06:00'
model: gpt-4-1106-preview
summary: "Imprimer un d\xE9bogage, c'est \xE9crire dans la console pour suivre ce\
  \ que fait votre programme."
title: "Affichage des sorties de d\xE9bogage"
weight: 33
---

## What & Why?
Imprimer un débogage, c'est écrire dans la console pour suivre ce que fait votre programme. On le fait pour repérer les bugs et comprendre le flux d'exécution.

## How to:
Imprimons "Hello, world!" et un nombre en boucle avec Arduino.

```Arduino
void setup() {
  Serial.begin(9600); // Démarre la communication série à 9600 bits par seconde
}

void loop() {
  Serial.println("Hello, world!"); // Affiche le texte
  for (int i = 0; i < 5; i++) {
    Serial.println(i); // Affiche les nombres de 0 à 4
    delay(1000); // Attends une seconde entre chaque nombre
  }
}
```

Sortie attendue:

```
Hello, world!
0
1
2
3
4
Hello, world!
0
...
```

## Deep Dive
Le débogage dans les années 1970 reposait sur des LEDs et des affichages à 7 segments. Arduino a popularisé `Serial.print()` grâce à sa facilité d'emploi. Beaucoup utilisent aussi des LED ou des écrans LCD comme alternative. Comment ça marche ? `Serial` crée une communication série entre l'Arduino et l'ordinateur. Utilisez `Serial.begin()` dans `setup()` et `Serial.print()` ou `Serial.println()` pour écrire des données.

## See Also
- Documentation Arduino sur la communication série: https://www.arduino.cc/reference/en/language/functions/communication/serial/
- Tutoriel pour débuter avec Arduino : https://www.arduino.cc/en/Guide/HomePage
- Des exemples de débogage avancé : http://playground.arduino.cc/Main/GeneralCodeLibrary
