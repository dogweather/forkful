---
title:                "Arduino: Konvertering av en dato til en streng"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være nødvendig å konvertere en dato til en streng for å kunne presentere informasjonen på en klar og forståelig måte i et Arduino-program. Dette kan være nyttig for å vise datoer på et display eller sende dem via seriekommunikasjon.

## Hvordan

Det finnes flere måter å konvertere en dato til en streng på, men en enkel løsning er å bruke funksjonen `String()`. Her er et eksempel på hvordan dette kan gjøres i et Arduino-program:

```Arduino
#include <DateTime.h> // Bibliotek for å håndtere dato og tid

DateTime now(2020, 12, 31, 23, 59, 0); // Oppretter en dato og tid

String datoStreng = String(now.year()) + "/" + String(now.month()) + "/" + String(now.day()); // Konverterer datoen til en streng

Serial.println(datoStreng); // Skriver ut datoen på serieporten
```

Kjører du dette programmet, vil du få følgende utskrift på serieporten:

`2020/12/31`

Som du kan se, vil datoen bli presentert i riktig format som en streng. Her er det viktig å huske at du må inkludere biblioteket "DateTime.h" for å kunne bruke funksjonene for dato og tid i Arduino.

## Dypdykk

Det finnes flere måter å konvertere en dato til en streng på, som for eksempel å bruke `snprintf()` eller `sprintf()`. Disse funksjonene gir mer kontroll over formateringen av datoen, men kan være mer kompliserte å bruke. Det er derfor opp til deg å vurdere hvilken metode som passer best for ditt prosjekt.

En annen nyttig funksjon for å konvertere en dato til en streng er `ToString()`, som er en del av "DateTime" biblioteket. Denne funksjonen gjør det enklere å formatere datoen og kan også håndtere datoer før år 2000.

## Se også

- [DateTime bibliotek](https://www.arduinolibraries.info/libraries/date-time)
- [Konvertere variabler til strenger i Arduino](https://www.arduino.cc/reference/en/language/functions/conversion/strl/)

Takk for at du leste denne bloggposten. Vi håper den var nyttig for deg i å konvertere datoer til strenger i dine Arduino-prosjekter. Lykke til med kodingen!