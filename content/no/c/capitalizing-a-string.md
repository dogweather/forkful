---
title:                "C: Å gjøre en streng stor forbokstav"
simple_title:         "Å gjøre en streng stor forbokstav"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvis du noensinne har jobbet med tekstbehandling eller dataprogrammering, har du sannsynligvis støtt på behovet for å konvertere en tekststreng til store bokstaver. Dette kan være nyttig for formatering, søkefunksjonalitet eller brukertilpasning, og derfor er det viktig å vite hvordan man kan gjøre dette i C-programmering.

## Hvordan
Du kan enkelt konvertere en tekststreng til store bokstaver ved hjelp av standard C-funksjoner. Her er et eksempel på hvordan dette kan gjøres:

```C
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main(){
    char tekst[] = "dette er en tekststreng";
    
    printf("Original tekst: %s\n", tekst);
    
    for(int i = 0; i < strlen(tekst); i++){
        tekst[i] = toupper(tekst[i]);
    }
    
    printf("Konvertert tekst: %s\n", tekst);
    
    return 0;
}
```

Dette programmet bruker funksjonen `toupper` fra standardbiblioteket <ctype.h> for å konvertere hver bokstav i teksten til store bokstaver. Ved hjelp av en for-løkke og funksjonen `strlen` fra <string.h> kan vi gå gjennom teksten og gjøre endringene.

Output:
```
Original tekst: dette er en tekststreng
Konvertert tekst: DETTE ER EN TEKSTSTRENG
```

## Dypdykk
I tillegg til funksjonen `toupper`, finnes det andre alternative måter å konvertere en tekststreng til store bokstaver på. En annen måte er ved hjelp av funksjonen `strlwr` fra <string.h>, som konverterer alle bokstaver til små bokstaver.

Det er også viktig å merke seg at disse funksjonene kun fungerer på ASCII-tegn, og derfor vil de ikke fungere på spesialtegn eller bokstaver fra andre alfabeter. For å håndtere disse bokstavene, må man bruke mer avanserte metoder og algoritmer.

## Se også
- [Offisiell dokumentasjon for toupper funksjonen](https://www.cplusplus.com/reference/cctype/toupper/)
- [Offisiell dokumentasjon for strlwr funksjonen](https://www.cplusplus.com/reference/cstring/strlwr/)
- [En artikkel om konvertering av tekster i C](https://www.geeksforgeeks.org/converting-strings-numbers-cc/)