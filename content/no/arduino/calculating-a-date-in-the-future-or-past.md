---
title:                "Arduino: Beregning av en fremtidig eller fortidig dato"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Hvorfor

Mange Arduino-prosjekter innebærer å jobbe med tid og dato, enten det er å aktivere en handling på et bestemt tidspunkt eller å holde oversikt over tidsrammer. Derfor er det nyttig å kunne beregne datoer i fortiden eller fremtiden ved hjelp av Arduino-kodene dine.

# Hvordan

Det første trinnet for å beregne en dato i fortiden eller fremtiden er å bestemme en referansedato. Det kan være dagens dato eller en annen vilkårlig dato. Deretter må du velge en hendelse eller handling som skal skje på en bestemt dato i fremtiden eller fortiden. Etter det er det bare å bruke enkle beregninger for å finne den ønskede datoen.

```Arduino
#include <TimeLib.h> // importerer tidslibrary

// Setter opp referansedato
int day = 29;
int month = 10;
int year = 2020;

// Velg hendelse på en bestemt dato
int eventDay = 25;
int eventMonth = 12;
int eventYear = 2020;

// Beregner antall dager mellom referansedato og hendelsesdato
int daysBetween = (Date.daysTo( eventDay, eventMonth, eventYear ) );
```

Nå kan vi bruke variabelen "daysBetween" for å aktivere en handling på den valgte datoen. For eksempel kan det være å sende en melding om at det er jul på en bestemt dato.

# Dypdykk

For å utføre mer komplekse beregninger, kan du bruke funksjoner fra TimeLib-biblioteket. For eksempel kan du bruke "age" -funksjonen for å beregne alderen til en person basert på fødselsdatoen deres. Du kan også bruke "dayOfWeek" -funksjonen for å finne ut hvilken ukedag en bestemt dato faller på.

```Arduino
#include <TimeLib.h>  // importerer tidslibrary

int birthDay = 25;
int birthMonth = 12;
int birthYear = 1995;

// Beregner alderen til personen
int age = (TimeLib.age( birthDay, birthMonth, birthYear ) );

// Finner ut hvilken ukedag fødselsdagen faller på
String dayOfWeek = (TimeLib.dayOfWeek( birthDay, birthMonth, birthYear ) );
```

Når du bruker disse funksjonene, er det viktig å sørge for at du har riktig format for datoene og variablene dine. Det kan gjøres ved å bruke konverteringsfunksjoner som "dateString" og "monthShortStr" fra TimeLib-biblioteket.

# Se også

- TimeLib Library: https://www.arduino.cc/reference/en/libraries/timelib/
- Arduino Date and Time Tutorial: https://www.arduino.cc/en/Tutorial/BuiltInExamples/DateTime
- Calculate Age in Arduino: https://www.hackster.io/Naushad_UzZ/google-sheets-esp8266-timezone-pn532-weather-client-83ce87