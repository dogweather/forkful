---
title:                "Confronto tra due date"
html_title:           "Elixir: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Che cos'è & perché?

Comparare due date significa controllare quale tra di loro è più recente o più vecchia. Questo è utile quando si programmatori vogliono tracciare un evento, come quando un prodotto è stato acquistato o quando è scaduto.

## Come fare:

Qui ci sono alcuni esempi sui come confrontare due date utilizzando Arduino. Si utilizza la funzione `time.h` per ottenere le informazioni sulla data corrente.

```Arduino
#include <Time.h>

void setup() {
  Serial.begin(9600); 
  setTime(23,59,55,31,12,2021); 
}

void loop(){ 
  if (now() > setTime(23,59,59,31,12,2021)) { 
      Serial.println("La data odierna è successiva alla data impostata."); 
  } else {
      Serial.println("La data odierna è precedente alla data impostata."); 
  }
  delay(1000);
}
```

L'output del codice sopra sarà:

```
La data odierna è successiva alla data impostata.
```

## Approfondimenti

Confrontare le date è una pratica comune nel mondo della programmazione fin dai primi giorni dei computer. Anche se ci sono alternative come l'uso delle librerie `RTClib.h` per Arduino, la funzione built-in `time.h` è una soluzione semplice ed efficiente.

La comparazione delle date in Arduino è basata sulle epoch Unix, contando i secondi trascorsi dal 1° gennaio 1970. Le date sono quindi rappresentate come numeri interi a 32 bit, rendendo semplice il loro confronto.

## Per approfondire:

Per più informazioni su come comparare le date in Arduino, puoi consultare i seguenti link:

- Un articolo utile su come utilizzare la libreria [Time](https://www.makerguides.com/time-arduino-library/) con Arduino.
- Un esempio pratico di come comparare le date in un tutorial di [Arduino Project Hub](https://create.arduino.cc/projecthub/SurtrTech/ds1307-real-time-clock-brick-rtc-7b4189).