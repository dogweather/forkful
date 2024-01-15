---
title:                "Arbeid med csv"
html_title:           "C: Arbeid med csv"
simple_title:         "Arbeid med csv"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen ønske å jobbe med CSV? Svaret er enkelt - CSV (Comma Separated Values) er en populær og enkel måte å lagre og dele tabulære data på. Det er et vanlig format som mange datafiler bruker, og det er derfor viktig å forstå hvordan man kan håndtere det i programmeringsspråk som C.

## Hvordan gjøre det

Det første trinnet for å jobbe med CSV i C er å inkludere "stdio.h" biblioteket. Dette vil tillate deg å bruke funksjoner som "printf" og "scanf" for å lese og skrive data.

Neste steg er å åpne filen som inneholder CSV-data. Dette gjøres ved hjelp av "fopen" funksjonen, som tar to argumenter - navnet på filen og lese/ skrive modus. For å lese CSV-data, bruk "r" som modus. For eksempel:
```C
FILE *fp = fopen("data.csv", "r");
```

Nå er det på tide å bruke "fgets" funksjonen for å lese og lagre data i en array. For å gjøre dette, må du først opprette en tom array for å lagre dataen. Deretter bruker du "fgets" funksjonen til å lese hver linje i filen og lagrer den i arrayen.

Etter at du har lagret alle linjene i arrayen, kan du bruke "sscanf" funksjonen til å hente ut hver verdi fra hver linje. For eksempel, hvis en linje i CSV-filen ser ut som dette: "Bil,Merke,Modell,År", kan du bruke "sscanf" funksjonen til å hente ut hver verdi ved hjelp av for eksempel:
```C
sscanf(line, "%[^,],%[^,],%[^,],%d", car, brand, model, year);
```

Til slutt, husk å lukke filen ved hjelp av "fclose" funksjonen når du er ferdig med å lese dataene.

Etter at du har lært hvordan du kan lese CSV-data, kan du også bruke samme metode til å skrive data til en CSV-fil. I stedet for "fopen" og "fgets" bruker du "fprintf" funksjonen for å skrive data til filen.

## Dykk dypere ned

For å jobbe med CSV-data i C, er det viktig å forstå at dataene er lagret som tekststrenger og at det er komma som skiller hver verdi. Dette betyr at du må være nøye med å bruke riktig "format specifiers" i "sscanf" funksjonen for å hente ut de riktige verdiene fra hver linje.

En annen ting å huske på er at CSV-filer kan inneholde forskjellige typer data, som for eksempel tekst og tall. Derfor må du sørge for å bruke riktig "data type" når du lagrer dataene i arrayen din eller når du skriver til filen.

## Se også
- [Offisielt C-nettsted](https://www.iso.org/standard/74528.html)
- [Dokumentasjon for fopen-funksjonen](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.h
tm)
- [Grunnleggende om å jobbe med tekstfiler i C](https://www.programiz.com/c-programming/c-file-input-output)