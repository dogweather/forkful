---
date: 2024-01-20 17:55:29.215497-07:00
description: "Come Fare: Tradizionalmente, la lettura degli argomenti da riga di comando\
  \ \xE8 una pratica comune nei sistemi operativi come Linux, Windows o macOS, ma\u2026"
lastmod: '2024-04-05T21:53:44.458053-06:00'
model: gpt-4-1106-preview
summary: "Tradizionalmente, la lettura degli argomenti da riga di comando \xE8 una\
  \ pratica comune nei sistemi operativi come Linux, Windows o macOS, ma Arduino opera\
  \ diversamente."
title: Lettura degli argomenti della riga di comando
weight: 23
---

## Come Fare:
```Arduino
// Attenzione: gli esempi mostrano come utilizzare gli argomenti passati attraverso una seriale in Arduino, non la riga di comando tradizionale

void setup() {
  // Inizia la comunicazione seriale a 9600 bps
  Serial.begin(9600);
}

void loop() {
  // Controlla se ci sono dati disponibili da leggere
  if (Serial.available() > 0) {
    // Legge il prossimo byte disponibile
    String argument = Serial.readStringUntil('\n');
    Serial.print("Hai inserito: ");
    Serial.println(argument);
  }
}

/*
  Esempio di output quando si invia "accendiLED" dalla console seriale:
  
  Hai inserito: accendiLED
*/
```

## Approfondimento:
Tradizionalmente, la lettura degli argomenti da riga di comando è una pratica comune nei sistemi operativi come Linux, Windows o macOS, ma Arduino opera diversamente. Non abbiamo una vera e propria "riga di comando", ma utilizziamo la seriale per comunicare con dispositivi esterni. Negli anni, molte alternative più sofisticate si sono sviluppate per la gestione degli input esterni negli sketch Arduino, tra cui l'uso di pulsanti fisici, sensori o connessioni a internet.

## Vedi Anche:
- [Arduino Reference: Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Arduino Project Hub](https://create.arduino.cc/projecthub) - Contiene progetti con esempi di comunicazione seriale.
- [Arduino Forum](https://forum.arduino.cc/) - Una comunità di persone che possono rispondere a domande più complesse sulla programmazione Arduino.
