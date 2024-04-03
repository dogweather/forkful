---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:15:21.120974-07:00
description: "Att skriva till standardfel (stderr) i Go inneb\xE4r att styra felmeddelanden\
  \ eller diagnostik som inte \xE4r avsedda f\xF6r huvudutdatastr\xF6mmen. Programmerare\u2026"
lastmod: '2024-03-13T22:44:37.409871-06:00'
model: gpt-4-0125-preview
summary: "Att skriva till standardfel (stderr) i Go inneb\xE4r att styra felmeddelanden\
  \ eller diagnostik som inte \xE4r avsedda f\xF6r huvudutdatastr\xF6mmen."
title: Skriva till standardfel
weight: 25
---

## Hur man gör:
I Go tillhandahåller `os`-paketet värdet `Stderr`, som representerar filen för standardfel. Du kan använda den med funktionerna `fmt.Fprint`, `fmt.Fprintf` eller `fmt.Fprintln` för att skriva till stderr. Här är ett enkelt exempel:

```go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Skriver en enkel sträng till stderr
    _, err := fmt.Fprintln(os.Stderr, "Det här är ett felmeddelande!")
    if err != nil {
        panic(err)
    }

    // Formaterat felmeddelande med Fprintf
    errCount := 4
    _, err = fmt.Fprintf(os.Stderr, "Processen slutfördes med %d fel.\n", errCount)
    if err != nil {
        panic(err)
    }
}
```

Exempel på utdata (till stderr):
```
Det här är ett felmeddelande!
Processen slutfördes med 4 fel.
```

Kom ihåg, dessa meddelanden kommer inte att visas i den vanliga utdatan (stdout) utan i felströmmen, som kan omdirigeras separat i de flesta operativsystem.

## Fördjupning
Konceptet med standardfel är djupt rotat i Unix-filosofin, som tydligt skiljer mellan normal utdata och felmeddelanden för en effektivare databehandling och hantering. I Go omfamnas denna konvention genom `os`-paketet, som ger direkt åtkomst till filbeskrivarna för stdin, stdout och stderr.

Även om det är lämpligt att skriva direkt till `os.Stderr` för många applikationer, erbjuder Go även mer avancerade loggningspaket som `log`, vilka erbjuder ytterligare funktioner såsom tidsstämpling och mer flexibla utdatakonfigurationer (t.ex. skrivning till filer). Att använda `log`-paketet, särskilt för större applikationer eller där mer omfattande loggningsfunktioner behövs, kan vara ett bättre alternativ. Det är också värt att notera att Gos tillvägagångssätt för felhantering, som uppmuntrar till att returnera fel från funktioner, kompletterar praxis att skriva felmeddelanden till stderr, vilket möjliggör en mer granulär kontroll av felhantering och rapportering.

I grund och botten, medan skrivning till stderr är en grundläggande uppgift i många programmeringsspråk, erbjuder Gos standardbibliotek och designprinciper både enkla och avancerade vägar för hantering av felutdata, i linje med bredare branschpraxis samtidigt som det tillgodoser Gos specifika designetos.
