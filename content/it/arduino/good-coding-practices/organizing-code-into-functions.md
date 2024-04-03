---
date: 2024-01-26 01:08:48.003032-07:00
description: "Organizzare il codice in funzioni significa spezzettare il tuo codice\
  \ in blocchi riutilizzabili, ognuno dei quali svolge un compito specifico. I\u2026"
lastmod: '2024-03-13T22:44:43.687966-06:00'
model: gpt-4-1106-preview
summary: Organizzare il codice in funzioni significa spezzettare il tuo codice in
  blocchi riutilizzabili, ognuno dei quali svolge un compito specifico.
title: Organizzazione del codice in funzioni
weight: 18
---

## Come fare:
Immagina di voler far lampeggiare un LED. Senza funzioni, il tuo `loop` è un groviglio disordinato. Con le funzioni, è ordinato. Ecco come fare:

```Arduino
const int LED_PIN = 13;

void setup() {
  pinMode(LED_PIN, OUTPUT);
}

void loop() {
  blinkLED(500); // Fa lampeggiare il LED ogni 500ms
}

// Funzione per far lampeggiare un LED
void blinkLED(int delayTime) {
  digitalWrite(LED_PIN, HIGH);
  delay(delayTime);
  digitalWrite(LED_PIN, LOW);
  delay(delayTime);
}
```

Output di esempio: Il tuo LED lampeggia felicemente, e lo scopo del codice è chiaro a colpo d'occhio.

## Approfondimento
Prima delle funzioni, la programmazione era un viaggio su strada lineare; vedevi ogni insidia dall'inizio alla fine. Dopo le funzioni, è più come saltare fra voli - passi direttamente alle parti importanti. Storicamente, le subroutine (le prime funzioni) furono una rivoluzione nella programmazione, permettendo ai programmatori di evitare di ripetere se stessi - questo è il principio DRY, Don't Repeat Yourself (Non Ripetere Te Stesso). Alternative alle funzioni potrebbero includere le macro o l'uso di classi per la programmazione orientata agli oggetti (OOP). Il nocciolo della questione? Quando definisci una funzione, stai fornendo al compilatore uno schema per eseguire un compito. Con Arduino, spesso si definiscono funzioni void che agiscono come comandi semplici per un microcontrollore, ma le funzioni possono anche restituire valori, rendendole più versatili.

## Vedi anche
Per maggiori informazioni sulle funzioni, consulta questi link:

- Riferimento ufficiale delle funzioni Arduino: https://www.arduino.cc/reference/en/language/functions/
- Approfondisci il principio DRY: https://it.wikipedia.org/wiki/Don%27t_repeat_yourself
- Un ripasso sulla storia delle subroutine: https://it.wikipedia.org/wiki/Subroutine
