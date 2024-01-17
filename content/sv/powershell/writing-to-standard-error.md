---
title:                "Skriva till standardfel"
html_title:           "PowerShell: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Vad och varför?

Att skriva till standardfel, eller "standard error" på engelska, är en vanlig praxis inom programmering. Det innebär helt enkelt att man skriver ut felmeddelanden till den standardiserade felutmatning som finns tillgänglig i de flesta programmeringsspråk. Detta är en viktig del av felsökningen och kan hjälpa utvecklare att hitta och åtgärda fel i sina program.

# Hur gör man?

Det finns flera sätt att skriva till standardfel i PowerShell, men den vanligaste metoden är att använda cmdleten "Write-Error". Detta kan göras genom att helt enkelt skriva cmdleten och det meddelande du vill skriva ut, till exempel:

```PowerShell
Write-Error "Ett fel har uppstått."
```

Detta kommer skriva ut meddelandet till standardfel och avbryta körningen av scriptet.

Man kan också använda sig av PowerShell's inbyggda variabler `$error` och `$ErrorActionPreference` för att styra hur felmeddelanden ska hanteras. Till exempel kan man använda dessa variabler för att även skriva ut felmeddelanden när man använder tysta skriptningslägen.

# Deep Dive

Att skriva till standardfel är ett viktigt verktyg i felsökningen av program. Det ger utvecklare en möjlighet att få viktig information om eventuella fel som kan uppstå under körningen av ett program. Det här konceptet är inte unikt för PowerShell, utan är en princip som används i många andra programmeringsspråk också.

Det finns även alternativ till att skriva till standardfel, till exempel användning av loggning eller att skicka felmeddelanden till en extern tjänst. Men att skriva till standardfel är fortfarande en viktig del av felet i många situationer.

När det kommer till implementationen av att skriva till standardfel i PowerShell så är det viktigaste att ha i åtanke att det är en del av felhantering och att man bör se till att man alltid hanterar eventuella fel som kan uppstå på ett korrekt sätt.

# Se även

För mer information om att skriva till standardfel i PowerShell bör du kolla in följande länkar:

- https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-error
- https://docs.microsoft.com/en-us/powershell/scripting/developer/cmdlet/how-to-debug-with-write-error-if-flow-control?view=powershell-7