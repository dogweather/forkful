---
date: 2024-01-20 17:32:24.569574-07:00
description: "Come Fare: Il confronto di date su Arduino si appoggia spesso sulla\
  \ libreria TimeLib, che offre una gestione del tempo simile a quella del linguaggio\
  \ C.\u2026"
lastmod: '2024-04-05T22:50:57.497861-06:00'
model: gpt-4-1106-preview
summary: Il confronto di date su Arduino si appoggia spesso sulla libreria TimeLib,
  che offre una gestione del tempo simile a quella del linguaggio C.
title: Confronto tra due date
weight: 27
---

## Come Fare:
```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  
  // Imposta due date
  tmElements_t data1, data2;
  setTime(0, 0, 0, 1, 1, 2023); // 01 gennaio 2023
  breakTime(now(), data1);
  setTime(0, 0, 0, 2, 1, 2023); // 02 gennaio 2023
  breakTime(now(), data2);

  // Confronta le date
  if (makeTime(data1) < makeTime(data2)) {
    Serial.println("Data1 è precedente a Data2");
  } else if (makeTime(data1) > makeTime(data2)) {
    Serial.println("Data1 è successiva a Data2");
  } else {
    Serial.println("Le date sono uguali");
  }
}

void loop() {
  // Il confronto viene effettuato una sola volta
}
```
Sample Output:
```
Data1 è precedente a Data2
```

## Approfondimento
Il confronto di date su Arduino si appoggia spesso sulla libreria TimeLib, che offre una gestione del tempo simile a quella del linguaggio C. Nata dall'evoluzione delle prime funzioni di gestione del tempo su microcontrollori, è diventata uno standard de facto. Alternativamente, si possono confrontare timestamp UNIX, ma la TimeLib offre una semplificazione nell'uso e nella lettura del codice. Dettagli di implementazione comprendono la conversione delle strutture `tmElements_t` in timestamp per il confronto, e la gestione di fusi orari e DST potrebbe richiedere librerie aggiuntive come TimeZone.

## Vedi Anche
- Documentazione TimeLib: http://playground.arduino.cc/Code/Time
- Esempi e tutorial Arduino: https://www.arduino.cc/en/Tutorial/HomePage
- Documentazione ufficiale Arduino: https://www.arduino.cc/reference/en/
