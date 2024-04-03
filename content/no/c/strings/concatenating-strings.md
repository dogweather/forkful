---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:53:51.893251-07:00
description: "Hvordan: I C er strenger er arrayer av tegn som avsluttes med et nulltegn\
  \ (`\\0`). I motsetning til i h\xF8yere programmeringsspr\xE5k, tilbyr ikke C en\
  \ innebygd\u2026"
lastmod: '2024-03-13T22:44:41.262635-06:00'
model: gpt-4-0125-preview
summary: I C er strenger er arrayer av tegn som avsluttes med et nulltegn (`\0`).
title: "Sammensl\xE5ing av strenger"
weight: 3
---

## Hvordan:
I C er strenger er arrayer av tegn som avsluttes med et nulltegn (`\0`). I motsetning til i høyere programmeringsspråk, tilbyr ikke C en innebygd funksjon for strengsammensetning. I stedet bruker du `strcat()` eller `strncat()` funksjonene fra `<string.h>` biblioteket.

Her er et enkelt eksempel som bruker `strcat()`:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char destination[50] = "Hei, ";
    char source[] = "Verden!";

    strcat(destination, source);

    printf("%s\n", destination);  // Utdata: Hei, Verden!
    return 0;
}
```

`strcat()` funksjonen tar to argumenter: målstrengen (som må ha nok plass til å holde det sammensatte resultatet) og kildestrengen. Den legger deretter til kildestrengen til målstrengen.

For mer kontroll over antallet tegn som er sammensatt, er `strncat()` tryggere å bruke:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char destination[50] = "Hei, ";
    char source[] = "Verden!";
    int num = 3; // Antall tegn som skal legges til

    strncat(destination, source, num);

    printf("%s\n", destination);  // Utdata: Hei, Ver
    return 0;
}
```

Dette begrenser sammensetningen til de første `num` tegnene av kildestrengen, noe som hjelper med å forhindre bufferoverflyt.

## Dypdykk
Funksjonene `strcat()` og `strncat()` har vært en del av C-standardbiblioteket siden dets begynnelse, noe som reflekterer språkets lavnivå-natur som krever manuell håndtering av strenger og minne. I motsetning til mange moderne programmeringsspråk som behandler strenger som førsteklasses objekter med innebygde sammensetningsoperatorer (som `+` eller `.concat()`), krever C's tilnærming en dypere forståelse av pekere, minnetildeling, og potensielle farer som bufferoverflyt.

Selv om `strcat()` og `strncat()` er mye brukt, kritiseres de ofte for deres potensial til å skape sikkerhetsproblemer om de ikke brukes forsiktig. Bufferoverflyt, hvor data overskrider minnet som er tildelt, kan føre til krasj eller bli utnyttet for vilkårlig kodeeksekusjon. Som et resultat vender programmerere seg stadig mer til tryggere alternativer, slik som `snprintf()`, som gir mer forutsigbar oppførsel ved å begrense antallet tegn skrevet til målstrengen basert på størrelsen dens:

```c
char destination[50] = "Hei, ";
char source[] = "Verden!";
snprintf(destination + strlen(destination), sizeof(destination) - strlen(destination), "%s", source);
```

Denne metoden er mer langtekkelig, men betydelig tryggere og markerer en skift i C programmeringspraksis mot å prioritere sikkerhet og robusthet over korthet.

Til tross for disse utfordringene er strengsammensetning i C en grunnleggende ferdighet, avgjørende for effektiv programmering i språket. Å forstå nyansene og de tilknyttede risikoene er nøkkelen til å mestre C programmering.
