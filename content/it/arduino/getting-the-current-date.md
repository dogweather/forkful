---
title:    "Arduino: Ottenere la data corrente"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Quando si programma su Arduino, a volte può essere utile ottenere la data corrente. Ad esempio, si potrebbe voler creare un orologio in tempo reale o un sistema di registrazione dei dati che richiede la data e l'ora corretti. In questo post, impareremo come ottenere la data corrente utilizzando Arduino.

## Come procedere

Per ottenere la data corrente su Arduino, possiamo utilizzare la libreria "Time". Per prima cosa, assicuriamoci di avere questa libreria correttamente installata nel nostro ambiente di sviluppo Arduino.

Una volta installata, dobbiamo includere la libreria nel nostro sketch utilizzando la seguente istruzione:

```arduino
#include <Time.h>
```

Dopo aver incluso la libreria, possiamo utilizzare la funzione "now()" per ottenere un oggetto di tipo "tmElements_t". Questo oggetto contiene informazioni sulla data e l'ora correnti. Possiamo poi utilizzare le funzioni "year()", "month()", "day()", "hour()", "minute()" e "second()" per ottenere i valori specifici di ciascun elemento.

Ecco un esempio di codice che stampa la data e l'ora correnti:

```arduino
#include <Time.h>

void setup() {
  Serial.begin(9600);
  setSyncProvider(RTC.get); // Imposta la funzione di sincronizzazione per la libreria Time
}

void loop() {
  time_t t = now(); // Ottiene la data e l'ora correnti
  tmElements_t tm = breakTime(t); // Converte l'oggetto time_t in un oggetto tmElements_t
  // Stampa la data e l'ora correnti nel formato DD/MM/YYYY HH:MM:SS
  Serial.printf("%d/%d/%d %d:%d:%d", tm.Day, tm.Month, tm.Year, tm.Hour, tm.Minute, tm.Second);
  delay(1000); // Fai una pausa di un secondo prima di ripetere
}
```

L'output del codice sarà qualcosa del genere:

```
20/09/2021 15:45:30
```

## Approfondimento

Per ottenere in modo più preciso la data e l'ora correnti, possiamo utilizzare un modulo di tempo reale (RTC) collegato ad Arduino. Questi moduli sono dotati di una batteria di backup che mantiene l'orologio in funzione anche quando l'Arduino è spento o perde l'alimentazione.

Inoltre, possiamo anche giocare con le impostazioni di fuso orario utilizzando la funzione "setTimeZone()". Questo ci permette di impostare il fuso orario desiderato per visualizzare la data e l'ora correnti in modo corretto.

## Vedi anche

- Documentazione della libreria "Time": https://www.pjrc.com/teensy/td_libs_Time.html
- Tutorial su come utilizzare una libreria RTC con Arduino: https://www.arduino.cc/en/Tutorial/BuiltinExamples/RTC
- Guida all'utilizzo della funzione "setTimeZone()": https://www.arduino.cc/en/Reference/TimeSetTimeZone