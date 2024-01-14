---
title:                "Arduino: Converting una data in una stringa"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché
Molti progetti Arduino richiedono l'utilizzo delle date, ad esempio per registrare un evento specifico o per sincronizzarsi con altri dispositivi. Convertire una data in una stringa è un processo fondamentale per poter ottenere un formato leggibile per l'utente.

## Come fare
Per convertire una data in una stringa, è necessario prima definire la data utilizzando la funzione "RTC.adjust()" per impostare l'ora locale e poi utilizzare le funzioni "day()", "month()" e "year()" per estrarre il giorno, il mese e l'anno. Infine, si può utilizzare la funzione "sprintf()" per creare la stringa con il formato desiderato. Ecco un esempio di codice che converte la data in formato "dd/mm/yyyy":

```Arduino
#include <RTClib.h>

RTC_DS1307 rtc;

void setup() {
  rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  Serial.begin(9600);
}

void loop() {
  DateTime now = rtc.now();

  // Estrarre giorno, mese e anno
  int day = now.day();
  int month = now.month();
  int year = now.year();

  // Creare la stringa con il formato desiderato
  char date_str [10];
  sprintf(date_str, "%d/%d/%d", day, month, year);

  // Stampare la data convertita
  Serial.println(date_str);

  delay(1000);
}
```

Ecco un possibile output dei dati:

```
25/02/2021
```

## Approfondimenti
La funzione "sprintf()" è molto utile per creare stringhe con formati personalizzati. Per conoscere tutte le opzioni di formattazione disponibili, si consiglia di consultare la documentazione ufficiale di Arduino.

Inoltre, esistono diverse librerie disponibili che semplificano la conversione delle date in stringhe, come ad esempio la libreria "DateTime" o "TimeLib".

## Vedi anche
- [Documentazione ufficiale di Arduino](https://www.arduino.cc/reference/en/)
- [Libreria DateTime](https://github.com/PaulStoffregen/DateTime)
- [Libreria TimeLib](https://github.com/PaulStoffregen/TimeLib)