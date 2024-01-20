---
title:                "Convertire una data in una stringa"
html_title:           "Javascript: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Convertire una data in una stringa significa trasformare una struttura di dati "data" in una sequenza di caratteri, cioè una "stringa". Lo facciamo per visualizzare le date in modi leggibili agli umani o per memorizzare e manipolare le date in strumenti che accettano solo testo, come i database e i file di testo.

## Come si fa:

Per convertire una data in una stringa in Arduino, utilizzeremmo la funzione `sprintf()`. Ecco un esempio di base:

```Arduino
void setup() {
  Serial.begin(9600);
  while(!Serial) { }
  
  int day = 15;
  int month = 8;
  int year = 2021;
  
  char date[11];
  sprintf(date, "%02d/%02d/%d", day, month, year);
  Serial.println(date);
}

void loop() {}
```

L'output sarà: `15/08/2021`.

## Approfondimento

Historicamente, la necessità di convertire le date in stringhe è sorta con la diffusione di linguaggi di programmazione ad alto livello e sistemi di gestione dei database.

Come alternativa al `sprintf()`, Arduino ha una funzione chiamata `dtostrf()`. Questo converte valori float o double in stringhe, ma non è ideale per le date.

Dettagli di implementazione: `sprintf` in Arduino non supporta direttamente i formatatori di data e ora come `%Y` per l'anno, `%m` per il mese e `%d` per i giorni, ecc., come in altre implementazioni di C. Quindi, dobbiamo lavorare con i numeri come interi o come valori float/double.

## Vedere anche

2. [Funzioni di stringa C++](http://www.cplusplus.com/reference/cstdio/sprintf/)
3. [Documentazione DateTime Arduino](https://playground.arduino.cc/Code/DateTime/)