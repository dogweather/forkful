---
title:                "Leser en tekstfil"
html_title:           "C: Leser en tekstfil"
simple_title:         "Leser en tekstfil"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Du lurer sikkert på hvorfor du bør lese en tekstfil når du allerede vet hvordan du leser vanlige filer. Vel, tekstfiler er en spesiell type fil som inneholder ren tekst uten noen formatering. Det kan være nyttig for å lagre eller utveksle data på en enkel og lesbar måte.

## Hvordan

For å lese en tekstfil i C, må du følge noen enkle trinn:

  * Åpne filen ved hjelp av `fopen()` funksjonen og lagre den i en `FILE` variabel.
  * Bruk `fscanf()` eller `fgets()` funksjonene til å lese linje for linje fra filen.
  * Når du er ferdig med å lese, må du lukke filen ved hjelp av `fclose()` funksjonen.

Her er et eksempel på hvordan du kan lese og skrive ut innholdet i en tekstfil:

```C
FILE *fptr;
char buffer[50];

fptr = fopen("test.txt", "r");
while (fscanf(fptr, "%s", buffer) == 1) {
  printf("%s ", buffer);
}

fclose(fptr);
```

Dette koden vil åpne filen "test.txt" i lesemodus, lese hver ord og skrive det ut i terminalen. Husk at `fscanf()` funksjonen leser et ord av gangen, mens `fgets()` leser en hel linje. Du kan velge den som passer best for ditt formål.

## Dypdykk

Hvis du ønsker å forstå mer om hvordan tekstfiler fungerer og hvordan de kan være nyttige, bør du ta en titt på konseptene bak knyttet til filbehandling i C. Det finnes forskjellige måter å lese og skrive filer på, inkludert lese og skrive i binære filer og omgå buffere ved hjelp av `fread()` og `fwrite()` funksjoner. Forskning og eksperimentering vil hjelpe deg med å bli mer komfortabel med å jobbe med tekstfiler.

## Se Også

Her er noen nyttige ressurser for å lære mer om tekstfilbehandling i C:

  * [C File I/O](https://www.programiz.com/c-programming/c-file-input-output)
  * [The C Programming Language by Kernighan and Ritchie](https://www.amazon.com/Programming-Language-Brian-W-Kernighan/dp/0131103628)
  * [File Handling in C - A Beginner's Tutorial](https://beginnersbook.com/2015/02/c-file-io/)

Lykke til med å lese og håndtere tekstfiler i C!