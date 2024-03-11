---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:52:48.116673-07:00
description: "\xC5 gj\xF8re om en streng til store bokstaver i C inneb\xE6rer \xE5\
  \ konvertere f\xF8rste tegn i hvert ord i en gitt streng til stor bokstav hvis det\
  \ er en liten\u2026"
lastmod: '2024-03-11T00:14:14.857684-06:00'
model: gpt-4-0125-preview
summary: "\xC5 gj\xF8re om en streng til store bokstaver i C inneb\xE6rer \xE5 konvertere\
  \ f\xF8rste tegn i hvert ord i en gitt streng til stor bokstav hvis det er en liten\u2026"
title: Sette stor bokstav i en streng
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å gjøre om en streng til store bokstaver i C innebærer å konvertere første tegn i hvert ord i en gitt streng til stor bokstav hvis det er en liten bokstav. Programmerere utfører ofte denne operasjonen for å standardisere brukerinput for søk, sorteringsoperasjoner, eller visningsformål, noe som sikrer konsistens og lesbarhet på tvers av tekstdata.

## Hvordan gjøre det:

Å gjøre om en streng til store bokstaver i C krever en grunnleggende forståelse av tegnmanipulasjon og gjennomgang av strenger. Siden C ikke har en innebygd funksjon for dette, vil du vanligvis sjekke hvert tegn og justere stor-/små bokstaver ved behov. Nedenfor er en enkel implementering:

```c
#include <stdio.h>
#include <ctype.h> // For islower og toupper funksjoner

void capitalizeString(char *str) {
    if (str == NULL) return; // Sikkerhetssjekk
    
    int capNext = 1; // Flagg for å indikere om neste bokstav skal være stor bokstav
    for (int i = 0; str[i] != '\0'; i++) {
        if (capNext && islower(str[i])) {
            str[i] = toupper(str[i]); // Gjør om til stor bokstav
            capNext = 0; // Nullstill flagg
        } else if (str[i] == ' ') {
            capNext = 1; // Neste tegn skal være stor bokstav
        }
    }
}

int main() {
    char exampleString[] = "hello world. programming in c!";
    capitalizeString(exampleString);
    printf("Streng med store bokstaver: %s\n", exampleString);
    return 0;
}
```

Eksempel på utdata:
```
Streng med store bokstaver: Hello World. Programming In C!
```

Dette programmet traverserer strengen `exampleString`, sjekker hvert tegn om det skal være stor bokstav. `islower`-funksjonen sjekker om et tegn er en liten bokstav, mens `toupper` konverterer det til stor bokstav. Flagget `capNext` bestemmer om neste bokstav som møtes skal konverteres, blir satt etter hvert mellomrom (' ') som finnes, og i utgangspunktet for å gjøre om strengens første tegn til stor bokstav.

## Dypdykk

Teknikken demonstrert er grei, men mangler effektivitet for veldig store strenger eller når den utføres gjentatte ganger i ytelseskritiske applikasjoner. I historisk og implementasjonssammenheng innebærer strengmanipulasjon i C, inkludert kapitalisering, ofte direkte buffermanipulasjon, som reflekterer Cs lave nivå tilnærming og gir programmereren full kontroll over minne og ytelsesavveininger.

Det finnes alternative, mer sofistikerte metoder for å gjøre om strenger til store bokstaver, spesielt når man vurderer lokaler og unicode-tegn, hvor kapitaliseringsreglene kan avvike betydelig fra det enkle ASCII-scenariet. Biblioteker som ICU (International Components for Unicode) gir robuste løsninger for disse tilfellene, men introduserer avhengigheter og overhead som kanskje ikke er nødvendig for alle applikasjoner.

I tillegg, mens eksempelet som er gitt bruker C standardbibliotekfunksjonene `islower` og `toupper`, som er en del av `<ctype.h>`, er det viktig å forstå at disse fungerer innenfor ASCII-området. For applikasjoner som krever behandling av tegn utover ASCII, som å håndtere aksenttegn i europeiske språk, vil ytterligere logikk eller tredjepartsbiblioteker være nødvendig for å nøyaktig utføre kapitalisering.

Konklusjonen er at selv om metoden som er skissert er egnet for mange applikasjoner, er det avgjørende å forstå dens begrensninger og de alternativene som er tilgjengelige for å utvikle robust, internasjonalisert programvare i C.
