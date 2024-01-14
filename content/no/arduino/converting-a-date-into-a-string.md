---
title:    "Arduino: Omdanne en dato til en streng"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Hvorfor

Å konvertere en dato til en tekststreng er en viktig del av mange Arduino-prosjekter. Det kan være nyttig for å vise dato og tid på en LCD-skjerm, lagre data med tidsstempler, eller til og med opprette en logg over hendelser. Uansett hva prosjektet ditt er, kan du få nytte av å vite hvordan du konverterer en dato til en tekststreng.

# Hvordan

For å konvertere en dato til en tekststreng i Arduino, må vi bruke biblioteket "Time". Dette er et standard bibliotek som allerede er inkludert i Arduino-programvaren, så du trenger ikke å laste ned noe ekstra. Det første du må gjøre er å inkludere Time-biblioteket i koden din. Dette gjøres ved å legge til følgende linje på toppen av koden din:

```
#include <Time.h>
```

Deretter må vi deklarere en variabel av typen "tmElements_t". Dette er en spesiell variabel som brukes til å lagre og håndtere tidsinformasjon. Vi kan velge å navngi variabelen vår noe som "t" for å gjøre det enklere å referere til senere.

```
tmElements_t t;
```

Nå kan vi bruke funksjonen "breakTime" for å dele opp en dato og lagre informasjonen i vår variabel. Denne funksjonen tar inn følgende argumenter: år, måned, dag, time, minutter og sekunder. For eksempel, hvis vi ønsker å konvertere datoen 10/03/2021 klokken 14:30, ville vår kode se slik ut:

```
breakTime(2021, 3, 10, 14, 30, 0, t);
```

Til slutt kan vi bruke funksjonen "makeTime" for å kombinere vår variabel med tidsdataene og konvertere det til sekunder siden starten av UNIX-tiden (1. januar 1970).

```
time_t sekunder = makeTime(t);
```

Vi kan nå bruke funksjonen "String" for å konvertere våre sekunder til en tekststreng. Vi kan også velge et ønsket format for vår tekststreng ved å bruke funksjonen "toString" og legge til ønskede separatorer. For eksempel, hvis vi ønsker å vise datoen i formatet DD.MM.YYYY, ville vår kode se slik ut:

```
String dato = String(day(sekunder)) + "." + String(month(sekunder)) + "." + String(year(sekunder));
```

Nå har vi konvertert datoen vår til en tekststreng, og vi kan bruke den videre i koden vår.

# Dypdykk

Det er viktig å merke seg at Arduino ikke har innebygd støtte for å håndtere dato og tid på samme måte som en vanlig datamaskin. Derfor er det nødvendig å bruke Time-biblioteket for å konvertere og håndtere tid. For mer detaljert informasjon om hvordan biblioteket fungerer, kan du se på dokumentasjonen på Arduino-nettstedet eller lese gjennom kildekoden.

# Se også

- [Time Library - Arduino](https://www.arduino.cc/en/Tutorial/BuiltInExamples/Time)
- [Time - Arduino Referanse](https://www.arduino.cc/reference/en/libraries/time/#include)
- [Å jobbe med tid - Arduino Project Hub](https://create.arduino.cc/projecthub/tags/time)