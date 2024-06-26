---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:25.394640-07:00
description: "Hur man g\xF6r: I C tillhandah\xE5ller header-filen `<time.h>` n\xF6\
  dv\xE4ndiga funktioner och typer f\xF6r att arbeta med datum och tider. Funktionen\
  \ `time()` h\xE4mtar\u2026"
lastmod: '2024-03-13T22:44:38.393815-06:00'
model: gpt-4-0125-preview
summary: "I C tillhandah\xE5ller header-filen `<time.h>` n\xF6dv\xE4ndiga funktioner\
  \ och typer f\xF6r att arbeta med datum och tider."
title: "H\xE4mta aktuellt datum"
weight: 29
---

## Hur man gör:
I C tillhandahåller header-filen `<time.h>` nödvändiga funktioner och typer för att arbeta med datum och tider. Funktionen `time()` hämtar den aktuella tiden, medan `localtime()` konverterar denna tid till den lokala tidszonen. För att visa datumet använder vi `strftime()` för att formatera det som en sträng.

Här är ett grundläggande exempel:

```c
#include <stdio.h>
#include <time.h>

int main() {
    char buffer[80];
    time_t rawtime;
    struct tm *timeinfo;

    // Hämta den aktuella tiden
    time(&rawtime);
    // Konvertera den till lokal tid
    timeinfo = localtime(&rawtime);
    
    // Formatera datumet och skriv ut det
    strftime(buffer, 80, "Dagens datum är %Y-%m-%d", timeinfo);
    printf("%s\n", buffer);

    return 0;
}
```

Ett exempel på utdata kan se ut så här:

```
Dagens datum är 2023-04-12
```

## Fördjupning
Tidshantering i C, som underlättas av `<time.h>`, går tillbaka till språkets och UNIX-systemens tidigaste dagar. Det bygger kring datatypen `time_t`, som representerar den aktuella tiden som antalet sekunder sedan Unix-epoken (1 januari 1970). Även om detta är effektivt och universellt kompatibelt innebär det också att standardbibliotekets tidsfunktioner i C är inneboende begränsade av `time_t`'s räckvidd och upplösning.

Moderna applikationer, särskilt de som kräver tidsstämplar med hög upplösning eller handlar om datum långt in i framtiden eller förflutet, kan finna dessa begränsningar utmanande. Till exempel är problemet med år 2038 en känd illustration där system som använder ett 32-bitars `time_t` kommer att överflöda.

För mer komplex tid- och datumhantering vänder sig många programmerare till externa bibliotek eller de funktioner som tillhandahålls av operativsystemet. I C++, till exempel, erbjuder biblioteket `<chrono>` mer precisa och mångsidiga tidsmanipuleringsförmågor.

Trots dess begränsningar är enkelheten och allmängiltigheten hos C:s tidsfunktioner helt lämpliga för många applikationer. Att förstå dessa verktyg är grundläggande för C-programmerare och erbjuder en blandning av historisk programmeringskontext och praktisk, vardaglig nytta.
