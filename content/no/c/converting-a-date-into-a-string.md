---
title:                "Konvertere en dato til en streng"
html_title:           "C: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å konvertere en dato til en streng i programmering betyr å endre en datoverdi til en lesbar tekststreng. Programmerere gjør dette for mer brukervennlig visning, lagring av data, eller for å manipulere dem i tekstform.

## Hvordan du gjør det:
Dato til streng konvertering i C kan oppnås ved bruk av `strftime` funksjonen. Dette er en standardisert funksjon tilgjengelig i `time.h` biblioteket.

```C
#include <time.h>
#include <stdio.h>

int main() {
    char str[80];
    time_t t = time(NULL);
    struct tm *tmp = localtime(&t);

    if(tmp == NULL) {
        printf("Failed to execute localtime\n");
        return 1;
    }

    if(strftime(str, sizeof(str), "%d-%m-%Y %H:%M:%S", tmp) == 0) {
        printf("Failed to execute strftime\n");
        return 1;
    }

    printf("%s\n", str);
    return 0;
}
```
Når du kjører koden over, vil den skrive ut den gjeldende datoen og tiden i formatet "dd-mm-yyyy HH:MM:SS".

## Dypdykk
**Historisk kontekst:** `strftime` funksjonen har sitt opphav i C standardbiblioteket og har vært tilgjengelig helt siden C89/C90 standarden. Dette gjør det til en betrodd og utbredt løsning for dato-streng konvertering.

**Alternativer:** Det er også mulig å bruke sprintf-funksjonen for å konvertere en strukturert dato til en streng. Men `strftime` har fordelen at det håndterer validering med datasikring på en mer effektiv måte.

**Implementeringsdetaljer:** `strftime` fungerer ved å ta en formatteringstreng som definert av brukeren. Denne strengen inneholder forskjellige direktiver som begynner med `%`, fulgt av bokstaver som representerer forskjellige dato- og tidskomponenter.

## Se Også
For å lære mer om temaet, sjekk ut følgende ressurser:

1. C Standard Library referanse: strftime: https://www.cplusplus.com/reference/ctime/strftime/
2. "Mastering Algorithms with C" av Kyle Loudon.
3. "C Programming Absolute Beginner's Guide (3rd Edition)" av Greg Perry og Dean Miller.