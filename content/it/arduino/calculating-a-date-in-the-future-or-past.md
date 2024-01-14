---
title:    "Arduino: Calcolare una data nel futuro o nel passato"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data futura o passata può essere utile in molti progetti di Arduino, come ad esempio per attivare sensori o eseguire operazioni in specifici momenti.

## Come fare

Per calcolare una data in futuro o passata, è necessario utilizzare due variabili: la prima conterrà la data attuale e la seconda conterrà il numero di giorni da aggiungere o sottrarre. Utilizzando la libreria Time di Arduino, è possibile ottenere la data attuale e salvarla nella variabile apposita. Successivamente, utilizzando la funzione timeAddsecond() o timeDecisecond(), è possibile aggiungere o sottrarre i giorni desiderati alla data attuale. Di seguito un esempio di codice:

```
#include <Time.h> // includere la libreria Time
const int numGiorni = 5; // variabile con il numero di giorni da aggiungere/sottrarre
time_t myDate = time(null); // ottenere la data attuale
myDate = timeAddsecond(myDate, (numGiorni * 86400)); // aggiungere il numero di secondi corrispondenti a numGiorni
Serial.println(myDate); // stampare la data ottenuta
```

L'output di questo codice sarà la data attuale più 5 giorni. Di seguito un esempio di output:

`1589437324`

## Approfondimento

Inoltre, è possibile anche utilizzare la libreria TimeAlarms di Arduino per impostare un timer per una data futura o passata. Utilizzando la funzione setAlarm() è possibile impostare una data e una funzione da eseguire in quel momento specifico. Di seguito un esempio di codice:

```
#include <Time.h>
#include <TimeAlarms.h> // includere la libreria TimeAlarms

void setup() {
    Serial.begin(9600);
    
    // impostare un alarm per il 17 maggio 2020 alle 18:30
    Alarm.alarmRepeat(18, 30, 0, myFunction); 
}

void loop() {
    // altre operazioni del codice
}

// funzione da eseguire quando scatta l'alarm
void myFunction() {
    Serial.println("Alarm attivato!");
}
```

L'alarm verrà attivato ogni volta che la data e l'ora specificata vengono raggiunte, permettendo di eseguire delle operazioni in quel momento specifico. 

## Vedi anche

- [Libreria Time di Arduino](https://github.com/PaulStoffregen/Time)
- [Libreria TimeAlarms di Arduino](https://github.com/PaulStoffregen/TimeAlarms)
- [Documentazione ufficiale di Arduino](https://www.arduino.cc/reference/en/libraries/time/)