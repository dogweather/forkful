---
date: 2024-01-20 17:28:33.266953-07:00
description: "\xC5 kalkulere en dato i fremtiden eller fortiden handler om \xE5 ta\
  \ en startdato og legge til eller trekke fra dager, m\xE5neder eller \xE5r. Programmerere\
  \ bruker\u2026"
lastmod: 2024-02-19 22:05:00.343345
model: gpt-4-1106-preview
summary: "\xC5 kalkulere en dato i fremtiden eller fortiden handler om \xE5 ta en\
  \ startdato og legge til eller trekke fra dager, m\xE5neder eller \xE5r. Programmerere\
  \ bruker\u2026"
title: Beregning av en dato i fremtiden eller fortiden
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å kalkulere en dato i fremtiden eller fortiden handler om å ta en startdato og legge til eller trekke fra dager, måneder eller år. Programmerere bruker denne funksjonen for å håndtere alt fra utløpsdatoer på kuponger til å planlegge fremtidige hendelser.

## Hvordan gjøre det:

Arduino har ikke innebygd støtte for avansert dato-manipulasjon rett ut av boksen, men ved hjelp av `Time.h`-biblioteket kan vi komme et stykke på vei. Følgende eksempel viser hvordan vi kan legge til en dag til det nåværende tidspunktet:

```Arduino
#include <Time.h>

void setup() {
  Serial.begin(9600);
  setTime(1617745793); // Angi en starttidspunkt (UNIX epoch time)
}

void loop() {
  time_t nå = now(); 
  time_t enDagFrem = nå + SECS_PER_DAY; // Legg til 24 timer
  
  Serial.print("Nåværende tidspunkt: ");
  Serial.println(ctime(&nå));

  Serial.print("Ett døgn senere: ");
  Serial.println(ctime(&enDagFrem));

  delay(10000); // Vent i 10 sekunder før loopen gjentar seg
}
```

Utskriften vil være Unix-tid konvertert til lesbar dato og tid for både det nåværende tidspunktet og en dag frem i tiden.

## Dypdykk:

Å behandle datoer i programmering har lange tradisjoner, og måter å håndtere det på har utviklet seg betydelig. Historisk sett kunne dette være et komplekst problem på grunn av ulike kalendersystemer og håndtering av tidssoner og skuddår. I Arduino-sammenheng begrenser funksjonaliteten seg til det enkle, hovedsakelig på grunn av begrensede ressurser.

Alternativer for mer avansert dato-manipulasjon inkluderer biblioteker som 'TimeLib.h' og 'DateTime.h'. Disse lar deg gjøre operasjoner som å håndtere ulike tidssoner og mer komplekse beregninger, som å finne ut hvilken ukedag en gitt dato faller på.

Når du beregner en dato i fremtiden eller fortiden i et Arduino-prosjekt, må du være oppmerksom på systemklokka. Hvis du bruker `millis()` eller annen timing relatert til mikrokontrolleren, kan klokken drifte over tid. For mer nøyaktige prosjekter, bruk en ekstern Real Time Clock (RTC) modul.

## Se også:

- Arduino Time Library: https://www.arduino.cc/en/Reference/Time
- TimeLib Library: https://github.com/PaulStoffregen/Time
- DateTime Library: http://playground.arduino.cc/Code/DateTime
- Arduino Forum: https://forum.arduino.cc/
- Online Epoch Converter: https://www.epochconverter.com/
