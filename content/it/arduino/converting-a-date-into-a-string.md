---
title:    "Arduino: Convertire una data in una stringa"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Convertingire una data in una stringa è un'operazione molto comune nell'ambito della programmazione con Arduino. La possibilità di visualizzare una data in forma di testo può essere utile per diversi progetti, come ad esempio la creazione di un orologio o una stazione meteorologica.

## Come fare

Per convertire una data in una stringa, è necessario utilizzare la libreria "TimeLib.h" inclusa in Arduino. Questa libreria fornisce diverse funzioni utili per gestire date e orari.

```Arduino
#include <TimeLib.h> 

// Inizializzazione dell'oggetto "tmElements_t" per la gestione della data
tmElements_t data; 

// Impostazione della data e dell'ora
data.Year = 2021;
data.Month = 10;
data.Day = 5;
data.Hour = 15;
data.Minute = 30;
data.Second = 0;

// Conversione della data in una stringa
String dataString = String(monthShortStr(data.Month, "%b") + " " + data.Day + ", " + data.Year);

// Stampa della stringa nella seriale
Serial.println(dataString); // Output: Oct 5, 2021
```

La funzione "monthShortStr()" viene utilizzata per ottenere il nome abbreviato del mese, mentre i formati "%b" e "%Y" specificano rispettivamente il formato del nome del mese e dell'anno. La stringa "dataString" viene composta unendo questi elementi insieme.

## Approfondimento

Per una gestione più approfondita delle date, è possibile utilizzare le funzioni fornite dalla libreria "TimeLib.h". Ad esempio, la funzione "monthStr()" può essere utilizzata per ottenere il nome completo del mese, mentre la funzione "isLeapYear()" restituisce un valore booleano che indica se l'anno specificato è bisestile o meno.

Altre funzioni utili includono "dayOfWeekStr()" per ottenere il nome del giorno della settimana, "hourFormat12()" per utilizzare il formato orario a 12 ore e "UNIXtime()" per ottenere il valore UNIX timestamp della data e ora specificate.

## Vedi anche

- Documentazione ufficiale di "TimeLib.h": https://www.pjrc.com/teensy/td_libs_Time.html
- Esempi di utilizzo di TimeLib: https://www.arduino.cc/en/Tutorial/TPrintingIntegerVariables
- Come creare un orologio con Arduino: https://www.instructables.com/Arduino-Clock-With-Timer-and-Alarm/