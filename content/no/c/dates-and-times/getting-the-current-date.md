---
title:                "Få dagens dato"
aliases: - /no/c/getting-the-current-date.md
date:                  2024-02-03T17:57:23.453916-07:00
model:                 gpt-4-0125-preview
simple_title:         "Få dagens dato"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å hente den nåværende datoen i C innebærer å benytte seg av det standard C-biblioteket for å hente og formatere systemets nåværende dato og tid. Programmerere trenger ofte denne funksjonaliteten for logging, tidsstempling eller planleggingsfunksjoner i applikasjonene sine.

## Hvordan:

I C gir `<time.h>`-headeren de nødvendige funksjonene og typene for å arbeide med datoer og tider. `time()`-funksjonen henter den nåværende tiden, mens `localtime()` konverterer denne tiden til den lokale tidssonen. For å vise datoen bruker vi `strftime()` for å formatere den som en streng.

Her er et grunnleggende eksempel:

```c
#include <stdio.h>
#include <time.h>

int main() {
    char buffer[80];
    time_t råtid;
    struct tm *tidinfo;

    // Hent den nåværende tiden
    time(&råtid);
    // Konverter den til lokal tid
    tidinfo = localtime(&råtid);
    
    // Formater datoen og skriv den ut
    strftime(buffer, 80, "Dagens dato er %Y-%m-%d", tidinfo);
    printf("%s\n", buffer);

    return 0;
}
```

Eksempel på utskrift kan se slik ut:

```
Dagens dato er 2023-04-12
```

## Dypdykk

Håndteringen av tid i C, som tilrettelegges av `<time.h>`, går tilbake til de aller første dagene av språket og UNIX-systemene. Det er bygget rundt `time_t`-datatype, som representerer den nåværende tiden som antall sekunder siden Unix Epoch (1. januar 1970). Selv om dette er effektivt og universelt kompatibelt, betyr det også at det standard C-bibliotekets tidsfunksjoner er iboende begrenset av rekkevidden og oppløsningen til `time_t`.

Moderne applikasjoner, spesielt de som krever høyoppløselige tidsstempler eller håndterer datoer langt inn i fremtiden eller fortiden, kan finne disse begrensningene utfordrende. For eksempel er Year 2038-problemet en berømt illustrasjon der systemer som bruker en 32-bits `time_t` vil overflyt.

For mer kompleks håndtering av tid og dato, tyr mange programmerere til eksterne biblioteker eller funksjonalitetene som tilbys av operativsystemet. I C++, for eksempel, tilbyr `<chrono>`-biblioteket mer presise og allsidige tidshåndteringskapasiteter.

Tross sine begrensninger, er enkelheten og allestedsnærværen av Cs tidsfunksjoner mer enn egnet for mange applikasjoner. Å forstå disse verktøyene er fundamentalt for C-programmerere, og tilbyr en blanding av historisk programmeringskontekst og praktisk, hverdagslig nytte.
