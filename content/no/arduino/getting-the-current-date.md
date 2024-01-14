---
title:                "Arduino: Få nåværende dato"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Hvorfor

Har du noen gang lurt på hvordan du kan få den nåværende datoen til å vises på din Arduino? Det er mange grunner til at noen kan ønske å gjøre dette, kanskje som en del av et større prosjekt eller for personlig nytte. Uansett hva årsaken er, så er det enkelt å implementere og kan være veldig nyttig i mange situasjoner.

# Hvordan

For å få den nåværende datoen på Arduino, trenger du å bruke en klokke modul, som for eksempel en RTC (Real Time Clock). Det finnes flere forskjellige RTC-moduler på markedet, men i denne guiden vil vi bruke DS1307 RTC, da den er ganske enkel å forstå og implementere.

Først må du koble RTC-modulen til Arduino ved å koble VCC til 5V og GND til bakken. Deretter må du koble SDA til pinne A4 og SCL til pinne A5. Når tilkoblingene er gjort, kan du laste ned biblioteket "RTClib" og inkludere det i Arduino-skissen din ved å gå til "Sketch" > "Include Library" > "Manage Libraries" og søke etter "RTClib".

Deretter kan du kopiere og lime inn følgende kode i skissen din:

```
#include <RTClib.h> // inkluderer biblioteket for RTC
RTC_DS1307 rtc; // lager en RTC_DS1307 objekt

void setup () {
  Serial.begin(9600); // starter seriell kommunikasjon
  rtc.begin(); // starter RTC kommunikasjon
}

void loop () {
  DateTime now = rtc.now(); // lagrer nåværende tid og dato i en DateTime variabel

  // skriver ut datoen og tiden til seriell monitor
  Serial.print(now.year());
  Serial.print('/');
  Serial.print(now.month());
  Serial.print('/');
  Serial.print(now.day());
  Serial.print(' ');
  Serial.print(now.hour());
  Serial.print(':');
  Serial.print(now.minute());
  Serial.print(':');
  Serial.print(now.second());
  Serial.println();

  delay(1000); // venter 1 sekund mellom hver utskrift
}
```

Etter å ha lastet skissen på Arduino og åpnet seriell monitor, vil du se at nåværende dato og tid blir skrevet ut. Dette er et enkelt eksempel, men du kan også bruke denne informasjonen til å utføre andre handlinger, for eksempel å styre en digital klokke eller logge data.

# Dypdykk

I denne koden bruker vi funksjonen "now()" for å få nåværende dato og tid. Dette er en enkel måte å hente den nåværende informasjonen på, men det finnes også andre funksjoner som kan være nyttige.

For eksempel kan du bruke "dayOfWeek()" for å få dagen i uken som en numerisk verdi (1-7). Du kan også bruke "UNIXtime()" for å få tiden som en UNIX-tidsstempel, som er antall sekunder siden 1. januar 1970.

# Se også

- [DS1307 RTC-bibliotek](https://github.com/adafruit/RTClib)
- [Arduino DS1307 RTC tutorial video på norsk](https://www.youtube.com/watch?v=YZCZpD6CRSo) (av "Programmeringsverkstedet")