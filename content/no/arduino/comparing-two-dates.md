---
title:                "Arduino: Sammenligning av to datoer"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Hvorfor
Å sammenligne to datoer er en viktig del av programmering, og spesielt for Arduino. Dette kan være nyttig for å kontrollere hendelser og timing i prosjekter, som for eksempel å aktivere en sensor på et bestemt tidspunkt eller å sjekke om et tidspunkt har passert. I denne blogginnlegget vil jeg vise deg hvordan du kan sammenligne to datoer ved hjelp av Arduino.

# Hvordan 
For å sammenligne to datoer på Arduino trenger du å bruke biblioteket Time.h. Dette biblioteket lar deg hente og manipulere dato og tid på en enkel måte. Først må du inkludere biblioteket på toppen av koden din ved å legge til følgende linje:

```Arduino
#include <Time.h>
```

Deretter kan du definere to datoer som du ønsker å sammenligne ved hjelp av `tmElements_t` -funksjonen, som vil lagre dato og tid i et struct. For eksempel:

```Arduino
tmElements_t dato1 = {0, 30, 23, 15, 8, 2021}; // 23:30 15.08.2021
tmElements_t dato2 = {30, 59, 23, 15, 8, 2021}; // 23:59 15.08.2021
```

Deretter kan du bruke funksjonen `makeTime()` for å konvertere disse datoobjektene til et tall i sekunder, som kan sammenlignes. For eksempel:

```Arduino
time_t tid1 = makeTime(dato1); // 1629107400
time_t tid2 = makeTime(dato2); // 1629109140
```

Nå kan du bruke vanlige sammenligningsoperatører (f.eks. `<`, `>`, `==`) for å sammenligne disse to tallene.

```Arduino
if(tid1 < tid2) {
  Serial.println("Dato 1 er tidligere enn dato 2");
}
```

Du kan også bruke `time_t` datatype til å hente nåværende tid og sjekke om et bestemt tidspunkt har passert. For eksempel:

```Arduino
time_t nå = now();
time_t sluttTid = makeTime(0, 0, 0, 1, 1, 2022); // Sluttid på 01.01.2022 kl 00:00:00

if(nå > sluttTid) {
  Serial.println("Slutttiden har passert");
}
```

# Dypdykk
For å kunne sammenligne to datoer, må du forstå hvordan datatyper fungerer. Når vi bruker datatypene `tmElements_t` og `time_t` for å lagre dato og tid, lagres verdien som et tall i sekunder siden 1. januar 1970. Dette er kjent som "epoch time". Dermed kan du enkelt sammenligne to verdier ved hjelp av matematiske operasjoner.

Det er viktig å merke seg at denne metoden vil ikke fungere for datoer før 1970 eller etter 2038, da datatypen `time_t` bare kan lagre verdier innenfor dette tidsintervallet.

# Se Også
- [Time.h biblioteket sin dokumentasjon](https://www.pjrc.com/teensy/td_libs_Time.html)
- [Epoch Converter](https://www.epochconverter.com/) for å konvertere dato og tid til epoch time
- [Tutorial on Using and manipulating time on Arduino](https://www.geekstips.com/tutorial-how-to-use-time-and-manipulate-it-on-arduino/) for å lære mer om å bruke tid på Arduino.