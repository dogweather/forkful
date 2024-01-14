---
title:    "Arduino: Confrontare due date"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Se sei un appassionato di Arduino o stai iniziando a imparare a programmare con questa piattaforma, potresti trovarti confrontato con la necessità di dover confrontare due date. Questa operazione può sembrare difficile, ma in realtà è piuttosto semplice se si conoscono alcuni trucchi di programmazione. In questa guida, imparerai come confrontare efficacemente due date utilizzando la tua scheda Arduino.

## Come fare

Per confrontare due date con Arduino, devi innanzitutto conoscere il formato di data e ora utilizzato dalla scheda. Di default, Arduino utilizza il formato Unix Time, che conta i secondi trascorsi dallo 0:00 del 1 gennaio 1970. Tuttavia, esistono librerie di programmazione che permettono di convertire questo formato in un formato più comprensibile, come ad esempio la libreria Time.h.

Una volta che hai importato la libreria Time.h nel tuo sketch, puoi utilizzare le funzioni di questa libreria per ottenere la data e ora attuali e salvarle in variabili. Ad esempio, con la funzione `hour()` puoi ottenere le ore attuali e salvarle in una variabile `ora_attuale`.

```Arduino
#include <Time.h>

int ora_attuale = hour(); //salva le ore attuali in una variabile
```

Per confrontare due date, dovrai utilizzare operazioni condizionali come `if` o `switch`. Ad esempio, se vuoi confrontare una data con una data specifica, puoi utilizzare la funzione `dateIs()` della libreria Time.h. Questa funzione restituirà `true` se la data passata come argomento corrisponde alla data attuale.

```Arduino
#include <Time.h>

bool data_corrisponde = dateIs(31,12,2020); //confronta la data attuale con il 31 dicembre 2020
if(data_corrisponde) {
  //esegue questo codice se la data corrisponde
} else {
  //esegue questo codice se la data non corrisponde
}
```

## Approfondimento

Il confronto di date può diventare un'operazione piuttosto complessa se si devono considerare anche gli anni bisestili e i fusi orari. Per questo motivo, potresti voler utilizzare una libreria più avanzata, come TimeLib.h, che permette di gestire queste complicazioni in maniera più semplice e precisa.

Inoltre, se stai lavorando con date di un periodo di tempo molto lungo, potresti dover convertire il formato Unix Time in un formato leggibile dall'essere umano. In questo caso, potresti utilizzare la funzione `strftime()` della libreria Time.h, che ti permette di formattare la data come preferisci.

## Vedi anche

- [Documentazione ufficiale Arduino - Time.h](https://www.arduino.cc/en/Reference/Time)
- [TimeLib.h - Libreria avanzata per la gestione del tempo in Arduino](https://github.com/PaulStoffregen/Time)
- [Tutorial per la conversione del formato Unix Time in formato leggibile](https://www.arduino.cc/en/Tutorial/ConversionTime)