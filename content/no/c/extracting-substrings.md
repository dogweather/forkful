---
title:    "C: Uttrekking av delstrenger"
keywords: ["C"]
---

{{< edit_this_page >}}

## Hvorfor

Å hente ut delstrenger, eller substrings som det kalles på engelsk, er en viktig funksjon i C-programmering. Dette gjør det mulig å manipulere tekst på en mer presis og effektiv måte. I denne bloggposten skal vi se nærmere på hvorfor og hvordan man kan hente ut substrings i C.

## Hvordan

Å hente ut substrings i C krever bruk av et par funksjoner. Først må man velge hvilken del av teksten man ønsker å hente ut, deretter må man bruke funksjonen `strncpy()` for å kopiere delstrengen til et nytt sted i minnet. For å forklare dette bedre, la oss se på et eksempel:

```C
#include<stdio.h>
#include<string.h>

int main() {
    // Oppretter en tekststreng
    char tekst[] = "Dette er en tekststreng";

    // Lager en buffer for den nye delstrengen
    char buffer[20];

    // Henter ut en del av teksten
    strncpy(buffer, tekst + 11, 7);

    // Printer ut delstrengen og legger til avsluttende nulltegn
    printf("%s\0", buffer);

    return 0;
}
```

Når vi kjører dette programmet, vil vi få følgende utput:

`tekststre`

La oss nå forklare koden litt nærmere. Vi starter med å inkludere standard biblioteker `stdio.h` og `string.h`. Deretter oppretter vi en tekststreng `tekst` med teksten "Dette er en tekststreng". Vi lager også en buffer `buffer` for å lagre den nye delstrengen. I `strncpy()` funksjonen, spesifiserer vi at vi ønsker å starte å kopiere fra indeks 11 i `tekst` og at vi vil kopiere 7 tegn. Dette er fordi at arrays og strenger i C starter på indeks 0, så ved å legge til 11 på `tekst`overskrider vi "Dette er en " og ved å kopiere 7 tegn får vi "tekststre". Til slutt printer vi ut `buffer` og legger til et avsluttende nulltegn for å markere enden av delstrengen.

## Dykk dypere

For å hente ut delstrenger på en mer avansert måte, kan man bruke funksjonen `strtok()` som deler opp en stor tekststreng i mindre deler basert på en gitt delimiter (skillemerke). La oss se på et eksempel på dette:

```C
#include<stdio.h>
#include<string.h>

int main() {
    // Oppretter en tekststreng
    char tekst[] = "en, to, tre, fire, fem";

    // Første strtok() kall vil hente ut "en"
    char *del = strtok(tekst, ", ");

    // Går gjennom tekststrengen til strtok() returnerer NULL
    while (del != NULL) {
        printf("%s ", del);
        // På neste strtok() kall vil "to" bli hentet ut
        del = strtok(NULL, ", ");
    }

    return 0;
}
```

Utpå kjøre programmet vil vi få følgende output:

`en to tre fire fem`

I dette eksempelet bruker vi komma og mellomrom som delimiter, og hver gang `strtok()` finner en av disse, vil den returnere den neste delen av strengen. Vi printer ut hver del og går gjennom hele teksten til `strtok()` returnerer `NULL`, som er slutten på strengen.

## Se også

- [C-programmering: Arrays og strenger (w3schools.com)](https://www.w3schools.com/cpp/cpp_strings.asp)
- [String manipulation functions (geeksforgeeks.org)](https://www.geeksforgeeks.org/string-manipulation-in-c-without-using-in-built-function/)