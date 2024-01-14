---
title:                "C: Utvinning av delstrenger"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å utvinne substrings er en vanlig oppgave i programmering, spesielt når du jobber med tekstdata. Det kan være nødvendig å isolere og manipulere visse deler av en streng for å oppnå ønsket resultat. Ved å lære å ekstrahere substrings, vil du kunne utføre en rekke oppgaver innenfor programmering.

## Slik gjør du det

For å utvinne en substring fra en streng, må du bruke funksjonen "strncpy" i C-programmering. La oss si at du har følgende streng:

```C
char string[] = "Dette er en tekst"
```

Hvis du ønsker å utvinne strengen "en tekst", må du bruke følgende kode:

```C
char substring[20];
strncpy(substring, &string[10], 8);
printf("%s", substring);
```

I dette tilfellet vil "strncpy" fungere som en skreddersydd funksjon som kopierer de angitte tegnene fra den opprinnelige strengen til en ny streng. Characterene "en" starter på indeks 10 i den opprinnelige strengen, og det er derfor vi bruker denne indeksen som utgangspunkt for å kopiere de angitte tegnene. Output vil bli "en tekst".

## Dypdykk

Når du utvinner substrings, er det viktig å være oppmerksom på indeksering og antall tegn du ønsker å ekstrahere. Hvis du for eksempel vil utvinne ordet "er" fra den opprinnelige strengen, må du vite at "e" starter på indeks 7 og "n" er på indeks 8. Dette betyr at du må angi et antall tegn som er større enn 1 for "strncpy" for å kopiere hele ordet. Hvis du for eksempel skriver følgende kode:

```C
char substring[20];
strncpy(substring, &string[7], 3);
printf("%s", substring);
```

Vil output bli "er " med et mellomrom etter bokstaven "r". Dette skyldes at "strncpy" også kopierer mellomrom, som teller som et tegn.

## Se også

- [Strncpy - C standard library](https://www.tutorialspoint.com/c_standard_library/c_function_strncpy.htm)
- [Substring extraction using strncpy - GeeksforGeeks](https://www.geeksforgeeks.org/substring-extraction-using-strncpy-c/)
- [String Manipulation in C - GeeksforGeeks](https://www.geeksforgeeks.org/string-manipulation-in-c-2/)