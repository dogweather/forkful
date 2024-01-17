---
title:                "Å få nåværende dato"
html_title:           "C: Å få nåværende dato"
simple_title:         "Å få nåværende dato"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å få tak i den nåværende datoen er en vanlig oppgave i programmering. Datoen er viktig for å spore tidssensitive hendelser, planlegge oppgaver og sertifisere dokumenter. Programmere bruker ofte denne funksjonen for å få tak i dato- og tidsinformasjon for å bruke i sine programmer.

## Hvordan:
Det er flere måter å få tak i den nåværende datoen i C-programmering. Her er et eksempel på hvordan du kan gjøre det:

```
#include <stdio.h>
#include <time.h>

int main() 
{
    time_t now;
    time(&now); // få tak i nåværende tid
    
    printf("Nåværende dato og tid: %s", ctime(&now));
    
    return 0;
}
```

Når du kjører dette programmet, vil du få utskrift som viser den nåværende datoen og tiden:

```
Nåværende dato og tid: Wed Sep 8 12:48:41 2021
```

## Deep Dive:
Å få tak i den nåværende datoen i C er ikke alltid like enkelt som å bruke en innebygd funksjon. Tidligere implementeringer av C hadde ikke enkle måter å få tak i datoen på, og det måtte gjøres manuelt ved å beregne antall sekunder siden en forhåndsbestemt dato. Men med utviklingen av standard C-library, er det nå enklere å få tak i den nåværende datoen ved å bruke funksjoner som `time_t` og `ctime`.

Alternativt kan programmere også bruke en rekke tredjepartsbiblioteker for å få tak i datoen og tidspunktet på en enklere måte. Noen av disse bibliotekene inkluderer Boost.Date_Time og Poco DateTime. Disse bibliotekene gir vanligvis mer fleksibilitet og funksjonalitet for å håndtere tid og datoer.

## Se Også:
- [C time.h Bibliotek Referanse](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Boost.Date_Time Bibliotek Dokumentasjon](https://www.boost.org/doc/libs/1_77_0/doc/html/date_time.html)
- [Poco DateTime Klasse Referanse](https://pocoproject.org/docs/Poco.DateTime.html)