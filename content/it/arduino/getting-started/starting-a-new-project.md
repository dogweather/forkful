---
title:                "Avvio di un nuovo progetto"
aliases:
- /it/arduino/starting-a-new-project/
date:                  2024-01-20T18:02:45.058934-07:00
model:                 gpt-4-1106-preview
simple_title:         "Avvio di un nuovo progetto"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (Cos'è & Perché?)
Iniziare un nuovo progetto Arduino significa scrivere codice per far interagire hardware e software. I programmatori lo fanno per creare prototipi, imparare e divertirsi.

## How to: (Come fare:)
```Arduino
void setup() {
  // Inizializza il pin LED come output.
  pinMode(LED_BUILTIN, OUTPUT);
}

void loop() {
  // Accendi il LED
  digitalWrite(LED_BUILTIN, HIGH);
  // Pausa per un secondo
  delay(1000);
  // Spegni il LED
  digitalWrite(LED_BUILTIN, LOW);
  // Pausa per un secondo
  delay(1000);
}
```
Questo esempio accende e spegne il LED integrato ogni secondo.

## Deep Dive (Approfondimenti)
Arduino è nato in Italia nel 2005 per rendere l'elettronica accessibile a artisti e designer. Rispetto ad altre piattaforme come Raspberry Pi o micro:bit, Arduino brilla per la compatibilità hardware e un IDE semplificato. Spesso è la prima scelta per iniziare con l'elettronica. Il codice si scrive in C/C++ ed è strutturato in due funzioni principali: `setup()` e `loop()`. `setup()` configura una sola volta il progetto mentre `loop()` va in esecuzione ciclica. È possibile espandere la funzionalità utilizzando librerie e moduli aggiuntivi.

## See Also (Vedi Anche)
- [Sito Ufficiale di Arduino](https://www.arduino.cc/)
- [Tutorial Arduino](https://www.arduino.cc/en/Tutorial/HomePage)
- [Documentazione sul linguaggio di Arduino](https://www.arduino.cc/reference/en/)
