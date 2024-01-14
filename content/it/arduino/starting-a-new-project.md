---
title:    "Arduino: Avviare un nuovo progetto"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per iniziare un nuovo progetto di programmazione con Arduino. Forse vuoi creare una luce automatica per la tua casa, o un robot che può muoversi e raccogliere oggetti. O forse semplicemente vuoi imparare qualcosa di nuovo e divertente!

## Come fare

Per iniziare un nuovo progetto con Arduino, avrai bisogno di questi passi:

1. Acquista una scheda Arduino e i relativi componenti (ad esempio, sensori, motori, luci, ecc.).
2. Scarica l'IDE di Arduino e installalo sul tuo computer.
3. Connetti la tua scheda Arduino al computer tramite cavo USB.
4. Progetta il tuo circuito utilizzando una breadboard.
5. Scrivi il codice nel tuo IDE di Arduino e caricalo sulla scheda Arduino.
6. Testa il tuo circuito e il codice per assicurarti che funzioni come previsto.

Ecco un esempio di codice che fa accendere un LED quando viene premuto un pulsante:

```
#include <Arduino.h>

#define LED 13
#define BUTTON 2

void setup() {
  pinMode(LED, OUTPUT);
  pinMode(BUTTON, INPUT_PULLUP);
}

void loop() {
  if (digitalRead(BUTTON) == LOW) {
    digitalWrite(LED, HIGH);
  } else {
    digitalWrite(LED, LOW);
  }
}
```

## Approfondimenti

Per iniziare un nuovo progetto con Arduino, è importante avere una buona comprensione dei suoi componenti principali: la scheda, i pin, la breadboard e il codice. Inoltre, è utile avere familiarità con alcune nozioni di elettronica, come la resistenza e la corrente. Assicurati di avere sempre un buon manuale di riferimento per consultare quando hai delle domande.

## Vedi anche

- [Sito ufficiale di Arduino](https://www.arduino.cc/)
- [Guida completa all'IDE di Arduino](https://www.arduino.cc/en/Guide/HomePage)
- [Tutorial su Arduino di Adafruit](https://learn.adafruit.com/category/learn-arduino)
- [Forum su Arduino di Arduino.cc](https://forum.arduino.cc/)