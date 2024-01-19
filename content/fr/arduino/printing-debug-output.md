---
title:                "Imprimer la sortie de débogage"
html_title:           "Arduino: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

L'impression de messages de debogage, c'est afficher des informations de diagnostic pour aider à repérer et résoudre les problèmes. Les développeurs le font pour comprendre l'état interne de leur code pendant l'exécution.

## Comment faire:

Voici comment imprimer un message de débogage sur la console série de l'Arduino:

```Arduino
void setup() {
  Serial.begin(9600); 
}

void loop() {
  int val = analogRead(A0);
  Serial.println(val);
  delay(1000);
}
```

Ce script lira la valeur du port analogique A0 toutes les secondes et l'affichera sur la console série.

## Approfondissement

Historiquement, le débogage était un processus long et délicat avant que les logiciels de débogage deviennent courants. L'impression de messages de débogage est une alternative simple mais efficace pour comprendre ce qui se passe dans votre code, en particulier lorsque vous travaillez avec du matériel en temps réel comme Arduino. 

En ce qui concerne les détails de mise en œuvre, Les messages de débogage sont envoyés via le port série, qui utilise les broches de transmission (TX) et de réception (RX) du microcontrôleur Arduino. Votre ordinateur doit être connecté à Arduino via USB pour recevoir ces messages.

## Voir aussi

- [Arduino Serial Communication](https://www.arduino.cc/reference/fr/language/functions/communication/serial/)
- [Debugging With Arduino](https://learn.sparkfun.com/tutorials)