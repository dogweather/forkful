---
title:                "Utskrift av feilsøkingsresultat"
html_title:           "Arduino: Utskrift av feilsøkingsresultat"
simple_title:         "Utskrift av feilsøkingsresultat"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å skrive ut feilsøkingsutdata er prosessen med å vise tilleggsinformasjon som hjelper utviklere med å forstå hva som skjer i koden deres. Dette er et uvurderlig verktøy for å diagnostisere og løse problemer med programvaren.

## Hvordan å:

Her er et grunnleggende eksempel på hvordan du bruker `printf` for å skrive ut feilsøkingsutdata i C:

```C
#include<stdio.h>

int main() {
    int a = 5;
    printf("Debug: a = %d\n", a);
    return 0;
}
```

Når du kjører dette programmet, vil du se utdataen: "Debug: a = 5". Dette er verdien av variabelen 'a'. 

## Dyp Dykk:

Historien om feilsøkingsutskrifter er nesten like gammel som programmering selv. Tidligere, i dager med ponsede kort og kontrollpaneler, var 'printf' debugging det eneste alternativet.

Det finnes mange alternativer til 'printf' debugging, som for eksempel å bruke en interaktiv debugger. Når det er sagt, vil utskrift av feilsøkingsutdata alltid være en rask og enkel måte å sjekke koden din på. 

'printf' funksjonen er en del av standardbiblioteket i C og er tilgjengelig i de fleste C-dialekter. Den er veldig allsidig og kan brukes til å skrive ut en rekke datatyper.

## Se Også:

1. [Hvordan Bruke printf for Feilsøking](https://link.to.example1)
2. [Alternativer til printf-Feilsøking](https://link.to.example2)
3. [Historien om printf](https://link.to.example3)