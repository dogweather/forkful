---
title:    "C: Konvertere en streng til små bokstaver"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

"Ikke alle har like mye erfaring med programmering, og noen ganger er det enkleste å glemme grunnleggende konsepter. En av disse er å konvertere tekst til små bokstaver (lower case) i C-programmering. Selv om det høres ut som en enkel oppgave, kan det være en kilde til frustrasjon for mange nybegynnere. Derfor vil jeg i denne bloggposten gå gjennom hvorfor det er viktig å kunne konvertere en streng til lower case, hvordan man gjør det og dykke litt dypere inn i dette konseptet.

## Hvorfor
Det å kunne konvertere en streng til lower case kan være en svært nyttig ferdighet i C-programmering. Dette er spesielt nyttig hvis man jobber med brukerinndata, for eksempel bokstaver som er tastet inn av en bruker. Det er også viktig i situasjoner der man må sammenligne strenger, da dette sjekker om strengene er likestilte og ikke case-sensitive.

## Hvordan
I C-programmering finnes det flere måter å konvertere en streng til lower case på. En av de enkleste måtene er å bruke standardfunksjonen "tolower()" som tar inn en enkelt karakter og returnerer den samme karakteren i lower case. Dette kan man gjøre for hver enkelt karakter i strengen for å konvertere hele strengen. Her er et eksempel på bruk av "tolower()" funksjonen:

```C
#include <stdio.h>
#include <ctype.h>

int main() {
    char string[] = "Dette Er En Streng";

    for (int i = 0; i < strlen(string); i++) {
        string[i] = tolower(string[i]);
    }

    printf("Lower case streng: %s", string);
    return 0;
}
```
Dette vil gi følgende output:
```
Lower case streng: dette er en streng
```

En annen måte å konvertere en streng til lower case på er å bruke "strlwr()" funksjonen, som gjør det samme som "tolower()" funksjonen, men for hele strengen på en gang.

## Dypdykk
I C-programmering, og mange andre programmeringsspråk, blir karakterer representert ved hjelp av en tallverdi kalt ASCII-verdi (American Standard Code for Information Interchange). Store bokstaver og små bokstaver har forskjellige ASCII-verdier, for eksempel har 'A' en ASCII-verdi på 65, mens 'a' har en ASCII-verdi på 97. Derfor er det viktig å konvertere disse verdiene til riktig case for å få ønsket resultat.

Det er også viktig å være oppmerksom på at konvertering til lower case kan føre til feil i visse situasjoner. For eksempel kan det påvirke utskrift av strenger til skjerm, da noen operativsystemer og terminaler kan kjøre forskjellige skriftformater og skriver ut strenger som er konvertert til lower case annerledes.

## Se Også
- [tolower() funksjonen på cplusplus.com](http://www.cplusplus.com/reference/cctype/tolower/)
- [strlwr() funksjonen på cplusplus.com](http://www.cplusplus.com/reference/cstring/strlwr/)