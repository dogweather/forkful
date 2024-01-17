---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Arduino: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Calcolare una data nel futuro o nel passato è l'atto di determinare una data basandosi su un intervallo di tempo. I programmatori spesso eseguono questa operazione per gestire eventi o attività programmati, o per mostrare informazioni come date di scadenza o scadenze.

## Come:

```
ArduinoDateTime.begin();
// Definisci una data di riferimento (gg, mm, aaaa) 
int giorno = 12;
int mese = 11;
int anno = 2020;

// Definisci l'intervallo di tempo in giorni
int giorni_da_aggiungere = 30;

// Calcola la data futura
ArduinoDateTime.calcularDiasCalc(anno, mese, giorno, giorni_da_aggiungere);
// Output: 12 dicembre 2020 + 30 giorni = 11 gennaio 2021
```

## Approfondimento:

Ci sono diversi modi per calcolare una data nel futuro o nel passato nei programmi, ma la maggior parte utilizza un metodo matematico chiamato "aggiunta di giorni". Questo metodo è stato sviluppato da John Conway nel 1962 e si basa sul calendario giuliano. 

Un'alternativa a questo metodo è l'utilizzo di librerie esterne, come "Time.h" o "DateTime.h". Queste librerie semplificano il processo di calcolo di date e forniscono funzioni aggiuntive come la conversione tra fusi orari.

Per implementare il calcolo di una data nel futuro o nel passato in un programma Arduino, è necessario avere una buona comprensione della sintassi di base e delle funzioni matematiche. È anche importante considerare gli errori comuni, come la conversione di numeri interi in lettere o errori nel formato della data.

## Vedi anche:

- [Una guida dettagliata sul calcolo di date in Arduino](https://www.arduino.cc/en/Tutorial/CalculateTime)
- [Libreria Time.h](https://github.com/PaulStoffregen/Time)
- [Libreria DateTime.h](https://github.com/YorickBleijenberg/DateTime)