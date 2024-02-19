---
aliases:
- /sv/go/creating-a-temporary-file/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:55:37.462085-07:00
description: "Att skapa en tempor\xE4r fil i Go m\xF6jligg\xF6r skapandet av en icke-persistent\
  \ fil avsedd f\xF6r kortvarig anv\xE4ndning, fr\xE4mst f\xF6r uppgifter s\xE5som\
  \ att lagra\u2026"
lastmod: 2024-02-18 23:08:51.353692
model: gpt-4-0125-preview
summary: "Att skapa en tempor\xE4r fil i Go m\xF6jligg\xF6r skapandet av en icke-persistent\
  \ fil avsedd f\xF6r kortvarig anv\xE4ndning, fr\xE4mst f\xF6r uppgifter s\xE5som\
  \ att lagra\u2026"
title: "Skapa en tillf\xE4llig fil"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skapa en temporär fil i Go möjliggör skapandet av en icke-persistent fil avsedd för kortvarig användning, främst för uppgifter såsom att lagra mellanliggande data eller bistå i batchbearbetningsjobb. Programmerare använder denna funktion för att säkert hantera data utan att påverka det permanenta filsystemet eller behöva manuell rensning.

## Hur gör man:

I Go tillhandahöll `ioutil`-paketet ursprungligen verktyg för att skapa temporära filer. Dock främjade Go 1.16 användningen av funktionerna i `os`- och `io/ioutil`-paketen till mer organiserade platser. Nu föredras `os`- och `io`-paketen för hantering av temporära filer.

Här är en steg-för-steg-guide för att skapa, skriva till och radera en temporär fil:

1. **Skapa en Temporär Fil:**

Med funktionen `os.CreateTemp` kan du skapa en temporär fil. Om du inte specifierar en katalog använder den standardtempmappen för ditt operativsystem.

```go
package main

import (
    "io/ioutil"
    "log"
    "os"
)

func main() {
    tmpFile, err := ioutil.TempFile("", "example.*.txt")
    if err != nil {
        log.Fatal(err)
    }
    log.Printf("Skapade temporär fil: %s\n", tmpFile.Name())

    defer os.Remove(tmpFile.Name()) // Städa upp
}
```

2. **Skriv till den Temporära Filer:**

Att skriva till filen kan uppnås med `Write`-metoden eller andra skrivfunktioner från `io`- eller `bufio`-paketen.

```go
_, err = tmpFile.Write([]byte("Hej, Världen!"))
if err != nil {
    log.Fatal(err)
}
```

3. **Läs från den Temporära Filen:**

Läsning följer liknande mönster, genom att använda filens `Read`-metod, eller använda verktyg från `io`- eller `bufio`-paketen.

```go
data, err := ioutil.ReadFile(tmpFile.Name())
if err != nil {
    log.Fatal(err)
}
log.Printf("Lästa data: %s\n", string(data))
```

4. **Radera den Temporära Filen:**

Även om `defer os.Remove(tmpFile.Name())`-uttrycket vid skapandefasen säkerställer att den temporära filen raderas efter att programmet avslutas, kan uttrycklig borttagning hanteras vid behov.

Exempelutskrift:
```
2023/04/01 15:00:00 Skapade temporär fil: /tmp/example.123456.txt
2023/04/01 15:00:00 Lästa data: Hej, Världen!
```

## Fördjupning

Mekanismen bakom Go’s hantering av temporära filer har utvecklats. Inledningsvis sköttes skapandet av temporära filer övervägande av den nu inaktuella funktionen `ioutil.TempFile`, vilket återspeglar bredare trender inom programvaruutveckling mot säkrare och mer effektiva filhanteringspraxis. Steget att integrera dessa funktioner i `os`- och `io`-paketen med Go 1.16 markerar en bredare strävan efter att förenkla språkets standardbibliotek och uppmuntra användningen av mer enhetliga och sammanhållna API:er.

Även om användning av temporära filer är en vanlig och ofta nödvändig praxis inom programmering, är det viktigt att notera att för mycket beroende på dem för att lagra stora mängder data eller för långsiktiga uppgifter kan leda till prestandaproblem. Dessutom, när skapandet av temporära filer inte är strikt kontrollerat eller när de inte städas bort på ett adekvat sätt, kan det leda till resursläckage som kan påverka filsystemet negativt. I scenarier som kräver beständig lagring eller hanterar betydande datamängder, erbjuder alternativ såsom databaser eller datalagring i minnet ofta bättre prestanda och tillförlitlighet jämfört med temporära filer.
