---
title:                "Lecture des arguments de ligne de commande"
date:                  2024-01-20T17:55:28.080384-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Lire des arguments de ligne de commande permet à un programme Arduino de saisir des données externe dès son démarrage. C'est utile pour personnaliser le comportement du programme sans le modifier.

## Comment faire :
Arduino ne traite pas les arguments de ligne de commande de la même manière que des langages plus traditionnels sur des systèmes d'exploitation comme Windows, Linux ou macOS. Cependant, vous pouvez lire des données envoyées via le port série au démarrage.

```Arduino
void setup() {
  Serial.begin(9600); // Démarre la communication série
  while (!Serial) {
    ; // attend que la connexion série soit établie
  }

  if (Serial.available() > 0) {
    String incomingArgument = Serial.readStringUntil('\n'); // Lit une ligne
    Serial.print("Argument reçu : ");
    Serial.println(incomingArgument);
  }
}

void loop() {
  // Votre code principal ici
}
```

Envoie de données via le port série :
```
Argument reçu : VotreArgumentIci
```

## Plongée Profonde
Les arguments de ligne de commande sont couramment utilisés en programmation, notamment pour influencer le démarrage d'applications sur des systèmes d'exploitation classiques. Sur Arduino, le concept diffère car la plateforme n'a pas un système d'exploitation permettant une interaction classique. Une alternative consiste à utiliser le port série pour envoyer des données au programme Arduino lorsqu'il se lance. Cela permet de simuler la lecture d'arguments au démarrage.

Concernant les détails d'implémentation, la fonction `Serial.available()` vérifie si des données sont disponibles sur le port série. Ensuite, `Serial.readStringUntil('\n')` lit les données jusqu'au caractère de nouvelle ligne, que nous pouvons considérer comme la fin d'un argument.

## Voir aussi
- [Documentation de la communication série Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Guide de démarrage rapide Arduino](https://www.arduino.cc/en/Guide/HomePage)
- [Tutorial sur la réception de données série avec Arduino](https://www.arduino.cc/en/Tutorial/BuiltInExamples/SerialEvent)