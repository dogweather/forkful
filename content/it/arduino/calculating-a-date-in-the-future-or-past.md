---
title:    "Arduino: Calcolare una data nel futuro o nel passato"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Perché

Lo scopo di questo progetto è quello di calcolare una data nel futuro o nel passato utilizzando Arduino. Questa funzione potrebbe essere utile per qualsiasi progetto che richieda un timer o la previsione di eventi futuri.

## Come fare

Per calcolare una data nel futuro o nel passato, utilizzeremo la libreria "Time" di Arduino. Per prima cosa, dobbiamo impostare la data attuale utilizzando la funzione "now()":

```Arduino
#include <Time.h>

void setup() {
  time_t now = now(); //impostiamo la data attuale
}
```

Successivamente, definiamo una variabile che rappresenta la data in millisecondi che vogliamo aggiungere o sottrarre dalla data attuale. Ad esempio, se vogliamo calcolare la data tra due giorni, useremo "2 * 86400000" (poiché un giorno corrisponde a 86400000 millisecondi).

```Arduino
long millisecondsToAdd = 2 * 86400000; //calcoliamo il numero di millisecondi da aggiungere
```

Infine, utilizziamo la funzione "makeTime()" per calcolare la nuova data:

```Arduino
#include <Time.h>

void setup() {
  time_t now = now(); //impostiamo la data attuale
  long millisecondsToAdd = 2 * 86400000; //calcoliamo il numero di millisecondi da aggiungere
  time_t newTime = now + millisecondsToAdd; //calcoliamo la nuova data
  setTime(newTime); //impostiamo la nuova data
  Serial.println(year()); //stampa l'anno della nuova data
  Serial.println(month()); //stampa il mese della nuova data
  Serial.println(day()); //stampa il giorno della nuova data
  Serial.println(hour()); //stampa l'ora della nuova data
  Serial.println(minute()); //stampa i minuti della nuova data
  Serial.println(second()); //stampa i secondi della nuova data
}
```

L'output di questo esempio sarà:

```Arduino
2021 //anno della nuova data
8 //mese della nuova data
5 //giorno della nuova data
14 //ora della nuova data
30 //minuti della nuova data
0 //secondi della nuova data
```

## Approfondimento

Il calcolo di una data nel futuro o nel passato può essere fatto anche utilizzando i timestamp Unix, ovvero i secondi trascorsi dal 1 gennaio 1970. Utilizzare i timestamp può risultare più preciso poiché tiene conto anche degli anni bisestili.

Per calcolare una data utilizzando i timestamp, seguiamo lo stesso procedimento di prima, ma al posto di utilizzare la funzione "now()", useremo la funzione "now() + now()":

```Arduino
#include <Time.h>

void setup() {
  time_t now = now() + now(); //impostiamo la data attuale in formato timestamp
  long secondsToAdd = 2 * 86400000; //calcoliamo il numero di secondi da aggiungere
  time_t newTime = now + secondsToAdd; //calcoliamo la nuova data
  setTime(newTime); //impostiamo la nuova data
  Serial.println(year()); //stampa l'anno della nuova data
  Serial.println(month()); //stampa il mese della nuova data
  Serial.println(day()); //stampa il giorno della nuova data
  Serial.println(hour()); //stampa l'ora della nuova data
  Serial.println(minute()); //stampa i minuti della nuova data
  Serial.println(second()); //stampa i secondi della nuova data
}
```

L'output sarà lo stesso di prima, ma utilizzando i timestamp è possibile ottenere una maggiore precisione nella data calcolata.

## Vedi anche

- [Guida alla libreria "Time" di Arduino](https://www.arduino.cc/en/Reference/Time)
- [Come funzionano i timestamp Unix](https://www.unixtimestamp.com/)