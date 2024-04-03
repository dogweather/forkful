---
date: 2024-01-26 00:36:47.059117-07:00
description: "Come fare: Diciamo che l'Arduino sta leggendo un sensore che occasionalmente\
  \ pu\xF2 produrre valori fuori range. Ecco come potresti gestire la cosa."
lastmod: '2024-03-13T22:44:43.689881-06:00'
model: gpt-4-1106-preview
summary: "Diciamo che l'Arduino sta leggendo un sensore che occasionalmente pu\xF2\
  \ produrre valori fuori range."
title: Gestione degli errori
weight: 16
---

## Come fare:
Diciamo che l'Arduino sta leggendo un sensore che occasionalmente può produrre valori fuori range. Ecco come potresti gestire la cosa:

```Arduino
int sensorValue = analogRead(A0);

if (sensorValue >= 0 && sensorValue <= 1023) {
  // Il valore è nel range, procedi con l'elaborazione
  Serial.println(sensorValue);
} else {
  // Il valore è fuori range, gestisci l'errore
  Serial.println("Errore: Valore sensore fuori range.");
}
```
Esempio di Output:
```
523
Errore: Valore sensore fuori range.
761
```

## Approfondimenti
La gestione degli errori non è sempre stata così diretta. Nei primi giorni, gli sviluppatori spesso ignoravano gli errori, conducendo al temuto "comportamento indefinito". Con l'evolversi della programmazione, sono evoluti anche gli strumenti - ora disponiamo di eccezioni in molte lingue, ma nel mondo Arduino si rimane ancora un po' all'antica con il 'controlla-prima' a causa dei vincoli hardware e delle radici in C++.

Nella programmazione Arduino, spesso si incontrano istruzioni `if-else` per la gestione degli errori. Ma ci sono alternative: utilizzare la funzione `assert` per fermare l'esecuzione se una condizione fallisce o progettare dispositivi di sicurezza all'interno della configurazione hardware stessa.

Quando implementi la gestione degli errori, considera l'impatto dello stop del programma rispetto al lasciarlo continuare in uno stato predefinito o sicuro. C'è un compromesso, e la scelta giusta dipende dal potenziale danno delle interruzioni rispetto al funzionamento errato.

## Vedi anche
Approfondisci la rilevazione e la gestione degli errori con questi:

- Riferimento del linguaggio Arduino: https://www.arduino.cc/reference/en/
- Uno sguardo più approfondito alla gestione degli errori di Embedded Artistry: https://embeddedartistry.com/blog/2017/05/17/creating-a-circular-buffer-in-c-and-c/
- Gestione degli errori in C++: https://en.cppreference.com/w/cpp/error/exception

Questo dovrebbe darti la conoscenza e la fiducia per evitare le insidie degli errori nelle tue avventure con Arduino.
