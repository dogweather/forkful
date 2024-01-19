---
title:                "Ottenere la data corrente"
html_title:           "Java: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Ottenere la data corrente significa sapere quale data è nel momento specifico in cui viene eseguita la funzione. Questo è utile per i programmatori quando hanno bisogno di tenere traccia del tempo o per organizzare eventi in base alla data.

## Come fare:

Nell'Arduino, non è possibile ottenere direttamente la data corrente. Pertanto, è necessario utilizzare un modulo RTC (Real Time Clock) esterno, come il DS1307. Ecco un esempio di come fare:

```Arduino
#include <Wire.h>
#include "RTClib.h"

RTC_DS1307 rtc;

void setup () {
  Serial.begin(9600);
  
  if (! rtc.begin()) {
    Serial.println("Non riesco a trovare RTC");
    while (1);
  }

  if (! rtc.isrunning()) {
    Serial.println("RTC non è in esecuzione!");
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop () {
  DateTime now = rtc.now();
  
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.print(now.day(), DEC);
  Serial.println();
  
  delay(3000);
}
```

Nell'output seriale, vedrai la data corrente visualizzata ogni tre secondi.

## Approfondimento

Dal punto di vista storico, prima dell'introduzione dei moduli RTC, i programmatori usavano metodi complicati e imprecisi per tracciare il tempo. Oggi, con i moduli RTC come il DS1307, ottenere la data corrente è un'operazione molto più semplice.

Come alternativa al DS1307, si può utilizzare anche il modulo RTC DS3231, più accurato e con una batteria di backup integrata.

Dettagli di implementazione: In questa implementazione specifica, usiamo la libreria `RTClib` per interfacciarci con il modulo RTC. La funzione `rtc.now()` ritorna un oggetto `DateTime` che contiene la data e l'ora corrente.

## Vedi Anche:

Per saperne di più sul modulo RTC DS1307 e sulla libreria RTClib, visita i seguenti link:

1. [Modulo RTC DS1307](https://lastminuteengineers.com/ds1307-rtc-arduino-tutorial/)
2. [Libreria RTClib](https://github.com/adafruit/RTClib)