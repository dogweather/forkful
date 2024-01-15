---
title:                "Convertire una data in una stringa"
html_title:           "Arduino: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Convertire una data in una stringa può essere utile quando si vuole visualizzare la data in un formato specifico o quando si vuole inviarla a un dispositivo esterno che richiede una stringa come input.

## Come fare

Per convertire una data in una stringa su Arduino, è necessario prima definire la data come un oggetto di tipo `DateTime`. Quindi, si può utilizzare il metodo `toString()` per convertire la data in una stringa nel formato desiderato.

```Arduino
#include <RTClib.h> //libreria per gestire il modulo RTC

RTC_DS1307 rtc; //istanza del modulo RTC
DateTime data; //oggetto DateTime per gestire la data

void setup() {
  //inizializzazione del modulo RTC
  rtc.begin();

  //lettura della data attuale dal modulo RTC
  data = rtc.now();
}

void loop() {
  //conversione della data in una stringa nel formato "GG/MM/AAAA"
  String data_stringa = data.toString("DD/MM/YYYY");

  //stampa della data sulla seriale
  Serial.println(data_stringa);
}
```

In questo esempio, viene utilizzata la libreria `RTClib` per gestire il modulo RTC e il metodo `toString()` con il parametro `"DD/MM/YYYY"` per specificare il formato della stringa desiderato. È possibile utilizzare diversi formati di stringa, come ad esempio `"MM/GG/YYYY"`, `"YYYY-MM-DD"`, `"GG/MM/YYYY HH:MM:SS"` e molti altri.

## Approfondimento

Mentre il metodo `toString()` è utile per la conversione di una data in una stringa, è importante notare che l'utilizzo di questo metodo può essere dispendioso in termini di memoria. Questo perché il metodo alloca una nuova stringa ogni volta che viene chiamato, che può portare a problemi di memoria in progetti più complessi.

Un'alternativa è quella di utilizzare la funzione `sprintf()` per formattare la data in una stringa senza generare una nuova allocazione di memoria. Tuttavia, questa opzione richiede un po' più di codice e può essere più complessa da gestire per i principianti.

## Vedi anche

- [Documentazione RTClib](https://github.com/adafruit/RTClib)
- [Guida alla gestione delle date su Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/datetime/)
- [Tutorial su sprintf() in Arduino](https://www.arduino.cc/reference/en/language/functions/character-functions/sprintf/)