---
title:                "Sette stor bokstav i en streng"
html_title:           "C: Sette stor bokstav i en streng"
simple_title:         "Sette stor bokstav i en streng"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Bytting av små til store bokstaver i en streng innebærer å endre hver liten bokstav til en stor bokstav. Programmerere gjør dette for å standardisere datainngang, noe som gjør det lettere å sammenligne og sortere strenger.

## Hvordan:

Her gir vi deg en kodeeksempel og eksempelutdata:

```C
#include <stdio.h>
#include <ctype.h>
#include <string.h>

void capitalize(char str[]){
  int len = strlen(str);
  for(int i=0; i<len; i++){
    str[i] = toupper(str[i]);
  }
}

int main(){
  char str[] = "hello world!";
  printf("Original String: %s\n", str);
  capitalize(str);
  printf("Capitalized String: %s\n", str);
  return 0;
}
```

Eksempel utdata:
```
Original String: hello world!
Capitalized String: HELLO WORLD!
```

## Dypdykk

(1) Historisk vinkling: C programmeringsspråk ble opprettet på begynnelsen av 1970-tallet. Funksjonen `toupper()` er en innebygd funksjon inkludert i C standardbiblioteket som brukes for å konvertere små bokstaver til store bokstaver.

(2) Alternativer: Du kan også lage din egen funksjon for å konvertere små bokstaver til store bokstaver ved å bruke ASCII-verdier.

(3) Gjennomføringsdetaljer: Funksjonen `toupper()` gjør bruk av ASCII-verdiene til bokstavene. Den subtraherer 32 fra ASCII-verdien av en liten bokstav for å få dens tilsvarende store bokstav.

## Se også

1. [Standard C Library Functions](https://en.cppreference.com/w/c/string/byte)
2. [C Programming/Strings](https://en.wikibooks.org/wiki/C_Programming/Strings)