---
title:                "Omgjøring av en dato til en streng"
aliases:
- /no/c/converting-a-date-into-a-string.md
date:                  2024-02-03T17:54:03.664893-07:00
model:                 gpt-4-0125-preview
simple_title:         "Omgjøring av en dato til en streng"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/converting-a-date-into-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å konvertere en dato til en streng i C innebærer å oversette en datostuktur eller tidsstempel til et menneskelesbart format. Programmerere utfører ofte denne oppgaven for å vise datoer i loggfiler, brukergrensesnitt, eller når de lagrer datoer i et tekstbasert format som JSON eller CSV.

## Hvordan:

Funksjonen `strftime` fra biblioteket `<time.h>` brukes vanligvis til dette formålet. Den lar deg formatere dato og tid på en rekke måter ved å spesifisere format-spesifikatorer. Her er et raskt eksempel:

```c
#include <stdio.h>
#include <time.h>

int main() {
    char dateStr[100];
    time_t now = time(NULL);
    struct tm *ptm = localtime(&now);

    // Konverter dato & tid til streng (f.eks., "Wed Jun 30 21:49:08 2021")
    strftime(dateStr, sizeof(dateStr), "%a %b %d %H:%M:%S %Y", ptm);
    
    printf("Nåværende dato og tid: %s\n", dateStr);
    return 0;
}
```

Et eksempel på utdata kan se slik ut:

```
Nåværende dato og tid: Wed Jun 30 21:49:08 2021
```

Du kan tilpasse formatet ved å endre format-spesifikatorene som sendes til `strftime`. For eksempel, for å få datoen i formatet `YYYY-MM-DD`, ville du bruke `"%Y-%m-%d"`.

## Dypdykk

Funksjonen `strftime` og biblioteket `<time.h>` er en del av C Standard Library, som går tilbake til den opprinnelige ANSI C-standarden (C89/C90). Selv om denne tilnærmingsmåten er grei og støttet på mange plattformer, kan den virke lavnivå og tungvint sammenlignet med moderne programmeringsspråk som tilbyr mer intuitive dato- og tidsbiblioteker.

Det bør merkes at selv om tidsfunksjonene i C-standardbiblioteket er bredt støttet og relativt enkle å bruke, mangler de noen av de mer komplekse tidssone-manipulasjons- og internasjonaliseringsfunksjonene som finnes i biblioteker av nyere språk eller tredjeparts C-biblioteker som International Components for Unicode (ICU).

Imidlertid gjør `strftime`-funksjonens tilpasningsdyktighet og brede plattformstøtte den til et pålitelig og nyttig verktøy for datostrengskonvertering i C. Programmerere som kommer fra språk med høyere nivå datotid-biblioteker kan trenge å justere seg til dens lavnivå natur, men vil finne den bemerkelsesverdig kraftig og allsidig for formatering av datoer og tider for en rekke applikasjoner.
