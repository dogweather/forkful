---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:09.453436-07:00
description: "Att konvertera ett datum till en str\xE4ng i C inneb\xE4r att \xF6vers\xE4\
  tta en datumstruktur eller tidsst\xE4mpel till ett l\xE4sbart format f\xF6r m\xE4\
  nniskor. Programmerare\u2026"
lastmod: '2024-03-13T22:44:38.395076-06:00'
model: gpt-4-0125-preview
summary: "Att konvertera ett datum till en str\xE4ng i C inneb\xE4r att \xF6vers\xE4\
  tta en datumstruktur eller tidsst\xE4mpel till ett l\xE4sbart format f\xF6r m\xE4\
  nniskor. Programmerare\u2026"
title: "Omvandla ett datum till en str\xE4ng"
weight: 28
---

## Vad & Varför?

Att konvertera ett datum till en sträng i C innebär att översätta en datumstruktur eller tidsstämpel till ett läsbart format för människor. Programmerare utför ofta denna uppgift för att visa datum i loggar, användargränssnitt eller när de lagrar datum i textbaserade format som JSON eller CSV.

## Hur man gör:

Funktionen `strftime` från biblioteket `<time.h>` används vanligtvis för detta ändamål. Den låter dig formatera datum och tid på en mängd olika sätt genom att specificera format specifierare. Här är ett snabbt exempel:

```c
#include <stdio.h>
#include <time.h>

int main() {
    char dateStr[100];
    time_t now = time(NULL);
    struct tm *ptm = localtime(&now);

    // Konvertera datumet & tiden till en sträng (t.ex. "Wed Jun 30 21:49:08 2021")
    strftime(dateStr, sizeof(dateStr), "%a %b %d %H:%M:%S %Y", ptm);
    
    printf("Aktuellt Datum och Tid: %s\n", dateStr);
    return 0;
}
```

Ett exempel på utskrift kan se ut så här:

```
Aktuellt Datum och Tid: Wed Jun 30 21:49:08 2021
```

Du kan anpassa formatet genom att byta format specifierarna som skickas till `strftime`. Till exempel, för att få datumet i formatet `YYYY-MM-DD`, skulle du använda `"%Y-%m-%d"`.

## Fördjupning

Funktionen `strftime` och biblioteket `<time.h>` är en del av C:s standardbibliotek, som går tillbaka till den ursprungliga ANSI C-standarden (C89/C90). Även om detta tillvägagångssätt är rakt på sak och stöds på många plattformar, kan det verka lågnivå och omständligt jämfört med moderna programmeringsspråk som erbjuder mer intuitiva datum- och tidsbibliotek.

Man bör notera, även om C:s standardbibliotekes tidsfunktioner är brett stödda och relativt enkla att använda, saknar de vissa av de mer komplexa funktionerna för tidszonsmanipulation och internationalisering som finns i bibliotek för nyare språk eller tredjeparts C-bibliotek som International Components for Unicode (ICU).

Dock gör `strftime`-funktionens anpassningsförmåga och breda plattformsstöd den till ett pålitligt och användbart verktyg för datumsträngskonvertering i C. Programmerare som kommer från språk med högre nivå av datumtidsbibliotek kan behöva justera sig till dess lågnivånatur men kommer att finna det anmärkningsvärt kraftfullt och mångsidigt för att formatera datum och tider för en mängd olika tillämpningar.
