---
title:                "Opprettelse av midlertidig fil"
html_title:           "C: Opprettelse av midlertidig fil"
simple_title:         "Opprettelse av midlertidig fil"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å opprette midlertidige filer er en vanlig praksis i mange programmeringsspråk, inkludert C. Disse filene brukes til å lagre midlertidige data som kun er nødvendige i løpet av programmet og ikke trenger å være lagret permanent.

## Hvordan

For å opprette en midlertidig fil i C kan man bruke funksjonen "tmpfile()". Denne funksjonen returnerer en peker til en åpen midlertidig fil. For eksempel:

```C
#include <stdio.h>

int main(){
    FILE *fp;

    fp = tmpfile();
    if(fp == NULL){
        printf("Kunne ikke opprette midlertidig fil.");
    }
    else{
        fprintf(fp, "Dette er data som skal lagres i den midlertidige filen.");
        printf("Data ble skrevet til den midlertidige filen.");
    }
}
```

Output:

```
Data ble skrevet til den midlertidige filen.
```

## Dypdykk

Det er verdt å merke seg at den midlertidige filen vil bli slettet når programmet avsluttes. Dette kan være nyttig når man ønsker å lagre sensitive data som ikke skal lagres permanent på harddisken.

I tillegg kan man også opprette midlertidige filer ved hjelp av funksjonene "tmpnam()" og "mkstemp()". Disse funksjonene gir mer kontroll over lagringsplassering og filnavn.

## Se også

- [tmpfile() dokumentasjon](https://www.tutorialspoint.com/c_standard_library/c_function_tmpfile.htm) 
- [Mer om temporary filer i C](https://www.guru99.com/c-temporary-files.html) 
- [Mer om C programmering](https://www.programiz.com/c-programming)