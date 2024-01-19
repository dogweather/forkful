---
title:                "Søking og erstatning av tekst"
html_title:           "Lua: Søking og erstatning av tekst"
simple_title:         "Søking og erstatning av tekst"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Søk og Erstatt Tekst i C Programmering

## Hva & Hvorfor?

Søk og erstatt tekst refererer til den prosessen hvor du lokaliserer en bestemt tekststreng i et større datasett og bytter det ut med annen tekst. Dette er avgjørende for programmerere da det vesentlig hjelper ved behandling av tekstfiler, oppdatering av kode eller datatabeller.

## Hvordan:

La oss gå rett til koden. Vi vil bruke funksjonen `strstr` for å søke og funksjonen `strcpy` for å erstatte tekst. Se eksempelet nedenfor.

```C
#include<stdio.h>
#include<string.h>

#define MAX_RLEN 50

void search_replace(char *str, char *oldWord, char *newWord){
    char buffer[MAX_RLEN];
    char *ch;
 
    // Sjekker om det gamle ordet er i strengen
    while((ch = strstr(str, oldWord)) != NULL){
        // Backup pekeren
        strncpy(buffer, str, ch-str); 
        buffer[ch-str] = '\0';
        
        // Legg det nye ordet til bufferen
        strcat(buffer, newWord);
 
        // Legg resten av den opprinnelige strengen til bufferen
        strcat(buffer, ch+strlen(oldWord));
 
        // Kopier buffer til den opprinnelige strengen
        strcpy(str, buffer);
    }
}

int main(){
    char str[] = "Hei verden";
    char oldWord[] = "verden";
    char newWord[] = "Norge";

    search_replace(str, oldWord, newWord);

    printf("%s", str);

    return 0;
}
```

Kjører dette vil gi følgende utskrift:

`Hei Norge`

## Dyp Dykk

Historisk sett har søk og erstatt tekst vært en funksjon som har vært tilgjengelig på mange forskjellige høynivåspråk og tekstbehandlingsapplikasjoner. I C underbygger det mange funksjoner og er viktig for alle som jobber med store tekstfiler.

Alternativene til `strstr` og `strcpy` inkluderer funksjoner som `strchr` (søke etter første forekomst av en karakter) og `strrchr` (søke etter siste forekomst av en karakter), samt funksjoner som `strncat` og `strncpy`.

Disse funksjonene baserer seg på å streife gjennom hvert karakter av strengen, fra første til siste karakter, og sjekk med forekomsten av det gamle ordet. Dette gjør at 'søk og erstatstekst' fungerer i konstant tidsspredning.

## Se Også:

For mer info om tekstbehandling i C, sjekk ut:
1. [C-programmeringsspråk](https://no.wikipedia.org/wiki/C_(programmeringsspr%C3%A5k))
2. [C String-handling Library](https://www.tutorialspoint.com/c_standard_library/string_h.htm)