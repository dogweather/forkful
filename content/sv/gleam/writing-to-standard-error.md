---
title:                "Gleam: Skriva till standard fel"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standard error är en viktig del av programvaruutveckling. Genom att skriva till standard error kan man förstå och identifiera fel som uppstår under exekveringen av ett program.

## Hur man gör

För att skriva till standard error i Gleam kan man använda sig av funktionen io.stderr.write() som tar en sträng som argument och skriver ut den till standard error. Nedan följer ett exempel på hur man kan använda denna funktion:

```Gleam
let error_message = "Ett fel har uppstått."
io.stderr.write(error_message)
```

Detta kommer att skriva ut strängen "Ett fel har uppstått." till standard error. Vid exekvering av programmet kommer denna sträng att visas i terminalen, vilket gör det lättare att identifiera och lösa eventuella fel.

## Djupdykning

Att skriva till standard error kan också vara användbart när man vill spela in eller logga information om programmet. Genom att skriva till standard error istället för standard output kan man se till att denna information inte stör det normala programflödet.

Det finns också andra sätt att skriva till standard error i Gleam, som att använda funktionen io.stderr.print() som tar emot flera argument och skriver ut dem på en ny rad var. Det är också möjligt att använda sig av formateringssträngar för att anpassa utskriften.

## Se även

För mer information om Gleam och programmering, ta en titt på följande länkar:

- Gleam's officiella dokumentation: https://gleam.run/
- En tutorial för att komma igång med Gleam: https://gleam.run/getting-started/
- En guide för felsökning i Gleam-program: https://gleam.run/debugging/
- En artikel om hur man läser och skriver till filer i Gleam: https://gleam.run/reading-writing-files/