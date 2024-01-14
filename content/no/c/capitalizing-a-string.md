---
title:    "C: Stor bokstavering av en streng"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Du har sannsynligvis sett mange programmer som tar en tekststreng og returnerer den samme strengen, men med de første bokstavene store (store bokstaver som vanligvis finnes i starten av en setning eller i navn). Men hvorfor ville noen egentlig ville gjøre dette? På overflaten kan det virke som en liten og ubetydelig funksjon, men det er faktisk et viktig konsept i programmering.

## Hvordan gjøre det
En metode for å gjøre dette i C-programmering er ved å bruke en løkke for å iterere gjennom den opprinnelige strengen og endre store bokstaver til små bokstaver, mens bokstavene rundt (gitt at de er små bokstaver) blir endret til store bokstaver.

```C
#include <stdio.h>

// Funksjonen for å gjøre bokstaver store
void gjør_bokstaver_store(char *streng) {
    int i = 0;

    // Gå gjennom strengen til enden
    while (streng[i] != '\0') {
        // Hvis bokstaven er en liten bokstav
        if (streng[i] >= 'a' && streng[i] <= 'z') {
            // Endre til å være en stor bokstav
            streng[i] = streng[i] - 32;
        }
        i++;
    }
    // Skriv ut den endrede strengen
    printf("Den nye strengen er: %s\n", streng);
}

int main() {
    // Definer en streng
    char tekst[] = "dette er en testtekst";
    // Kall funksjonen for å gjøre bokstaver store
    gjør_bokstaver_store(tekst);
    return 0;
}
```

Dette vil gi følgende utskrift:

```
Den nye strengen er: DETTE ER EN TESTTEKST
```

## Dypdykk
En ting å merke seg er at denne metoden kun fungerer på engelske bokstaver, da det kun endrer bokstaver mellom 'a' og 'z'. I tillegg, dette er kun en måte å gjøre bokstaver store på, det finnes flere måter å oppnå samme resultat på. For eksempel kan man bruke innebygde funksjoner som `toupper()` og `tolower()` i `ctype.h` biblioteket for å konvertere bokstaver.

En annen ting er at det finnes en forskjell mellom å endre strengen og å bare skrive ut den endrede strengen. I eksempelet ovenfor endrer vi den opprinnelige strengen, men man kan også lage en funksjon som lager en kopi av den opprinnelige strengen og endrer kopien mens den opprinnelige forblir uendret.

## Se også
- [Konvertering av bokstaver i C](https://www.programiz.com/c-programming/library-function/ctype.h/toupper)
- [Strenger i C](https://www.programiz.com/c-programming/c-strings)