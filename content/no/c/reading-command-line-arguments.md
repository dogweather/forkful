---
title:                "C: Lesing av kommandolinje-argumenter"
simple_title:         "Lesing av kommandolinje-argumenter"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##Hvorfor

Når du jobber med C programmering, er det alskens ting du må ta i betrakning, forstå og implementere. Command line arguments er en av de tingene du bør være kjent med. Det er nyttig for både utvikling og testing av koden din. Derfor kan det være nyttig å forstå hva command line argumenter er og hvordan man bruker dem.

##Slik gjør du det

Å lese command line argumenter i C er enkelt og kan gjøres med få linjer med kode. Først trenger du en main-funksjon som tar i mot to parametere, argc og argv. "argc" er antall argumenter som blir levert til programmet, while argv er selve argumentene. Så hvordan leser vi disse argumentene? La oss se på et eksempel:

```C
#include <stdio.h>

int main(int argc, char *argv[])
{
  int i;

  for (i = 0; i < argc; i++) {
    printf("Argument %d: %s\n", i, argv[i]);
  }

  return 0;
}
```

Når du kjører programmet og inkluderer noen argumenter, vil du få en output som ligner på dette:

```
$ ./program arg1 arg2
Argument 0: ./program
Argument 1: arg1
Argument 2: arg2
```

Som du kan se, er argumentene lagret som en streng i "argv" og indexert fra 0 til antallet argumenter minus 1. Dette gjør det enkelt å lese og behandle argumentene i koden din.

##Dykk dypere

Det er viktig å merke seg at argumentene som blir levert til programmet ditt er strenger, uavhengig av hva slags type data som blir levert. Derfor må du være forsiktig hvis du prøver å konvertere dem til andre datatyper. En annen ting du bør være klar over er at argumentene blir levert i riktig rekkefølge, så du kan bygge logikk og logiske tester basert på dette.

En annen nyttig funksjon er å kunne lese argumentene på en måte som ikke er avhengig av rekkefølgen de blir levert i. Dette kan gjøres ved hjelp av argument flags. For eksempel kan du lage et flagg "-h" for å vise en hjelpetekst og et flagg "-v" for å vise versjonsinformasjon.

I tillegg kan du også bruke argumenter til å aktivere eller deaktivere visse deler av koden din, noe som kan være nyttig for å teste ulike funksjoner og scenarier.

##Se også

For mer informasjon om command line arguments i C, se følgende ressurser:

- [C Command Line Arguments](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [C Programming Tutorial: Command Line Arguments](https://www.learn-c.org/en/Command_Line_Arguments)
- [Command-Line Arguments in C](https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm)