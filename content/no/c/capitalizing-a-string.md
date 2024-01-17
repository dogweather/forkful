---
title:                "Stor bokstav i en streng"
html_title:           "C: Stor bokstav i en streng"
simple_title:         "Stor bokstav i en streng"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å kapitalisere en streng betyr å gjøre den første bokstaven stor, mens resten av bokstavene forblir små. Dette gjøres vanligvis for å følge konvensjoner og standarder i programmeringsspråket, og for å gjøre koden mer leselig for andre utviklere.

## Hvordan:
Eksempel 1:
```C
#include <stdio.h>
#include <string.h>
int main(){
    char str[] = "hallo verden";
    // bruker funksjonen toupper for å kapitalisere første bokstav i strengen
    str[0] = toupper(str[0]);
    printf("%s", str); // Output: Hallo verden
    return 0;
}
```

Eksempel 2:
```C
#include <stdio.h>
#include <ctype.h>
int main(){
    char str[] = "dette er en setning";
    for(int i=0; i<strlen(str); i++){
        // bruker funksjonen islower for å sjekke om bokstaven er liten
        if(islower(str[i])){
            // bruker funksjonen toupper for å kapitalisere bokstaven
            str[i] = toupper(str[i]);
        }
    }
    printf("%s", str); // Output: Dette er en setning
    return 0;
}
```

## Dypdykk:
I eldre versjoner av C var det vanlig å bruke funksjoner som toupper eller toupper_l for å kapitalisere en streng. Men i den nyeste versjonen av C har funksjonen toupper blitt erstattet av isupper for å gjøre koden mer effektiv og brukervennlig.

Et alternativ til å bruke en løkke og to funksjoner for å kapitalisere en streng, er å bruke funksjonen strupr, som også finnes i C. Denne funksjonen tar inn en streng og kapitaliserer alle bokstavene i den.

Implementeringen av strupr-funksjonen vil variere fra programmeringsspråk til programmeringsspråk, men konseptet er det samme. Den går gjennom hver bokstav i strengen og bruker en innebygd funksjon for å kapitalisere den.

## Se Også:
- [2 måter å kapitalisere en streng i C](https://www.hackerearth.com/practice/notes/capitalize-first-character-of-string-in-c/)
- [MarkDown av formell til uformell skriving](https://softwareengineering.stackexchange.com/questions/41614/markdown-for-formal-to-informal-writing)