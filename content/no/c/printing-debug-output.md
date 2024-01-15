---
title:                "Utskrift av feilsøkingsutdata"
html_title:           "C: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Når du skriver kode, er det viktig å kunne finne feil og problemer. En måte å gjøre dette på er å bruke "debug output," eller feilsøkningsutskrift, som hjelper deg med å spore gjennomgangen av programmet og finne hvor problemet ligger. I denne artikkelen skal vi se på hvordan man kan skrive ut debug output i C-programmer.

## Hvordan

For å skrive ut debug output i C, må du bruke funksjonen `printf()`. Denne funksjonen tar inn en streng som første argument, og eventuelt variabler etter det, og skriver det ut på skjermen. La oss se på et eksempel:

```C
#include <stdio.h>

int main(void) {
    int tall = 5;
    printf("Tallet er: %d\n", tall);
    return 0;
}
```

Dette programmet vil skrive ut "Tallet er: 5" på skjermen når det kjøres. Her har vi brukt `%d` for å angi at vi vil skrive ut en tall-verdi. For å skrive ut en streng, bruker vi `%s` i strengen vår og gir strengen vi vil skrive ut som et annet argument til `printf()`.

Du kan også skrive ut flere variabler ved å gi flere argumenter til `printf()`, og de vil bli skrevet ut i samme rekkefølge som de er gitt. Du kan lese mer om printf-funksjonen og de ulike spesifikatorene på [denne siden](https://www.tutorialspoint.com/c_standard_library/c_function_printf.htm).

## Deep Dive

I tillegg til å skrive ut variabler og strenger, kan du også inkludere mer detaljerte meldinger i debug output ved å bruke *format strings*. Disse er en blanding av fast tekst og variable som kan skrives ut på en spesiell måte, for eksempel ved bruk av spesifikatoren `%f` for å skrive ut flyttall.

Format strings er veldig nyttig når du trenger å skrive ut lange meldinger eller inkludere informasjon fra flere variabler i utskriften. Her er et eksempel på hvordan vi kan bruke en format string:

```C
#include <stdio.h>

int main(void) {
    int tall = 5;
    float desimaltall = 3.14;
    printf("Tallet er: %d og desimaltallet er: %.2f\n", tall, desimaltall);
    return 0;
}
```

Denne koden vil skrive ut "Tallet er: 5 og desimaltallet er: 3.14" på skjermen. Legg merke til at ved bruk av `%f`, har vi også brukt `.2` for å angi at vi kun vil ha to desimaler med i utskriften.

## Se også

For å lære mer om debugging og bruk av `printf()`-funksjonen, kan du sjekke ut følgende ressurser:

- [Debugging in C: Tips and Tricks](https://www.programiz.com/c-programming/debugging)
- [Debugging with GDB: A guide](https://www.gdbtutorial.com/)
- [The printf() Function](https://www.tutorialspoint.com/c_standard_library/c_function_printf.htm)