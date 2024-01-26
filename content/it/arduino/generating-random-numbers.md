---
title:                "Generazione di numeri casuali"
date:                  2024-01-20T17:48:19.103248-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generazione di numeri casuali"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Generare numeri casuali significa creare valori imprevisti durante l'esecuzione del programma. I programmatori li usano per giochi, simulazioni, test e quando serve un elemento di casualità.

## How to: (Come fare:)
```Arduino
void setup() {
  Serial.begin(9600);
  randomSeed(analogRead(0));
}

void loop() {
  int numeroCasuale = random(1, 100);  // Genera un numero casuale tra 1 e 99
  Serial.println(numeroCasuale);
  delay(1000);  // Attendi un secondo tra un numero e l'altro
}
```
Output esempio:
```
23
79
45
...
```

## Deep Dive (Approfondimento)
La funzione `random()` in Arduino genera numeri pseudo-casuali; non sono veramente casuali, ma abbastanza per molti scopi. I numeri vengono generati usando un algoritmo che li rende prevedibili se conosci il 'seme'. Ecco perché usiamo `randomSeed(analogRead(0))` per inizializzare il generatore di numeri casuali con una lettura di un pin analogico, che è abbastanza casuale da fungere da seme.

In passato, prima dell'avvento dei microcontrollori, la casualità veniva ottenuta con metodi analogici come dadi o sistemi meccanici, non esenti da problemi di bias e riproducibilità. In alternativa si usavano costosi generatori di rumore hardware.

Dal punto di vista dell'implementazione, ci sono metodi più sofisticati per la generazione di numeri casuali, come l'utilizzo di algoritmi crittograficamente sicuri, ma questi richiedono più risorse, generalmente non necessarie negli scenari semplici per cui sono progettati gli Arduino.

## See Also (Vedi Anche)
- [Documentazione ufficiale di Arduino - random()](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Guida alla funzione randomSeed()](https://www.arduino.cc/reference/en/language/functions/random-numbers/randomseed/)
- [Wikipedia - Generatore di numeri casuali](https://it.wikipedia.org/wiki/Generatore_di_numeri_casuali)
