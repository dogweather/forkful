---
title:                "Å finne lengden av en streng"
html_title:           "Arduino: Å finne lengden av en streng"
simple_title:         "Å finne lengden av en streng"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være nyttig å finne lengden på en streng når man jobber med tekst og ønsker å vite hvor mange tegn den inneholder. Dette kan være spesielt nyttig når man jobber med sensorer og ønsker å konvertere sensorverdier til tekst for å vise dem på en skjerm.

## Hvordan gjøre det

Det første trinnet for å finne lengden på en streng i Arduino er å definere selve strengen. Dette kan gjøres ved å bruke datatype `char` og inkludere variabelnavnet og verdien som ønskes, for eksempel: 

```Arduino
char navn[] = "John Doe";
```

Deretter kan man bruke funksjonen `strlen()` for å finne lengden på strengen. Denne funksjonen beregner lengden ved å telle antall tegn i strengen og returnerer dette som et heltall.

``` Arduino
int lengde = strlen(navn);
```

Man kan også kombinere disse trinnene i ett ved å direkte definere variabelen `lengde` som lengden på strengen, for eksempel:

```Arduino
int lengde = strlen("Arduino");
```

Etter å ha beregnet lengden, kan man bruke enkel programmering som for eksempel å skrive ut lengden på en LCD-skjerm eller å lagre verdien for senere bruk.

## Dypdykk

Det er viktig å merke seg at funksjonen `strlen()` kun beregner antall tegn i en streng, og ikke tar hensyn til mellomrom eller spesialtegn. For eksempel vil strengen "John    Doe" være 8 tegn lang selv om det er flere mellomrom mellom ordene. 

Det er også verdt å nevne at `strlen()` funksjonen kun fungerer med strenger definert med datatype `char`, og ikke med andre datatype som `String`.

For å unngå potensielle feil og komplikasjoner, kan det være lurt å inkludere et ekstra tegn, ofte kalt en "terminating null character", som indikerer slutten på strengen. Dette vil sikre at `strlen()` funksjonen returnerer riktig lengde. Et eksempel på dette kan være å definere strengen "Arduino", men inkludere et ekstra tegn i slutten som ikke synes, for eksempel "Arduino\0".

## Se også

* [Arduino String-Length](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strlen/)
* [C++ String Length](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)