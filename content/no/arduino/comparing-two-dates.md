---
title:    "Arduino: Sammenligning av to datoer"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang lurt på hvordan du kan sammenligne to forskjellige datoer ved hjelp av Arduino? Det kan være nyttig for å lage tidsstyrte prosjekter eller for å måle tidsintervaller. I denne bloggposten vil vi gå gjennom hvordan du kan gjøre dette ved hjelp av Arduino-programmering.

## Hvordan gjøre det

Først må vi inkludere "Time" biblioteket i koden vår. Dette gir oss tilgang til funksjoner for å håndtere datoer og tid i Arduino.

```Arduino
#include <TimeLib.h> 
```

Deretter kan vi bruke funksjonen "year()" for å få året fra en bestemt dato. For eksempel, hvis vi vil sammenligne to datoer og sjekke om de er fra samme år, kan vi skrive noe slik:

```Arduino
if (year(dato1) == year(dato2)) {
  // Koden din her
}
```

Du kan også bruke "month()", "day()" og "hour()" funksjonene på samme måte for å sammenligne andre deler av datoen som måned, dag og time.

```Arduino
if (month(dato1) == month(dato2)) {
  // Koden din her
}
```

Du kan også sammenligne to datoer og få tiden mellom dem ved hjelp av "difference()" funksjonen, som vil gi deg en verdi i millisekunder.

```Arduino
unsigned long forskjell = difference(dato1, dato2);
```

## Dypdykk

Når du sammenligner to datoer, er det viktig å merke seg at det kan være avvik på grunn av ulike tidsformater eller tidssoner. Derfor anbefales det å konvertere datoer til samme format og tidssone før du sammenligner dem.

En annen ting å huske på er at Arduino har en begrensning på datoen den kan håndtere. Den kan ikke håndtere datoer før 1. januar 1970 og etter 19. januar 2038.

## Se også

- [Time biblioteket dokumentasjon](https://www.arduino.cc/en/reference/time)
- [Video tutorial om å sammenligne datoer på Arduino](https://www.youtube.com/watch?v=5S-QGgK-5oU)
- [Mer avanserte funksjoner for å håndtere datoer i Arduino](https://www.arduinoplatform.com/arduino-date-time-functions/)

Takk for at du leste denne bloggposten og håper den hjelper deg med å sammenligne datoer ved hjelp av Arduino! Lykke til med dine kommende prosjekter!