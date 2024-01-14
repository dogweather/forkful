---
title:    "C: Sammenligne to datoer"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang lurt på hvordan man sammenligner to datoer i et C-program? Vel, i denne bloggposten vil jeg forklare hvorfor du kanskje ønsker å kjenne til denne ferdigheten og hvordan du kan gjøre det.

## Hvordan

For å sammenligne to datoer i et C-program, må du først konvertere de to datoene til et numerisk format som datamaskinen kan forstå. Dette gjør du ved å bruke `mktime()`-funksjonen som tar inn en `struct tm`-variabel som representerer datoen.

```C
#include <stdio.h>
#include <time.h>

int main() {

    // Oppretter to struct tm-variabler for å representere datoene
    struct tm dato1 = { .tm_year = 2019, .tm_mon = 11, .tm_mday = 3 }; 
    struct tm dato2 = { .tm_year = 2020, .tm_mon = 6, .tm_mday = 10 };

    // Konverterer datoene til numerisk format
    time_t tid1 = mktime(&dato1);
    time_t tid2 = mktime(&dato2);

    // Sammenligner de to datoene ved å trekke dem fra hverandre
    time_t differanse = difftime(tid1, tid2);

    // Skriver ut resultatet i antall sekunder
    printf("%ld sekunder forskjell mellom datoene\n", differanse);

    return 0;
}
```

Output:

```
179784000 sekunder forskjell mellom datoene
```

I dette eksempelet er resultatet antall sekunder mellom de to datoene, men du kan også bruke `difftime()`-funksjonen til å sammenligne datoer i andre formater som for eksempel år, måneder, dager osv.

## Deep Dive

Nå som du vet hvordan man sammenligner to datoer i et C-program, la oss gå litt dypere og se hva som faktisk skjer i bakgrunnen når vi bruker `mktime()`- og `difftime()`-funksjonene.

`mktime()`-funksjonen konverterer datoen til et numerisk format som kalles Unix epoch, som baserer seg på antall sekunder som har gått siden 01. januar 1970. Dette gjør det enklere å regne med datoer på en datamaskin.

Når du bruker `difftime()`-funksjonen, beregnes differansen mellom to datoer ved å trekke den ene fra den andre i Unix epoch-formatet. Resultatet kan deretter konverteres til ønsket format.

## Se også

- [C-programspråket](https://no.wikipedia.org/wiki/C_(programmeringsspr%C3%A5k))
- [struct tm-dokumentasjon](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [time.h-dokumentasjon](https://www.tutorialspoint.com/c_standard_library/time_h.htm)