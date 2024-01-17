---
title:                "'Arbeta med json'"
html_title:           "PowerShell: 'Arbeta med json'"
simple_title:         "'Arbeta med json'"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med JSON kan beskrivas som att läsa, skriva och behandla data i ett lättförståeligt format. Det är användbart för programmerare eftersom det tillåter dem att strukturera och dela data på ett enkelt sätt, vilket i sin tur underlättar vidare bearbetning och utbyte av information.

## Så här gör du:
För att arbeta med JSON i PowerShell kan du använda dig av cmdleten `ConvertTo-Json` för att konvertera data till JSON-format, eller `ConvertFrom-Json` för att konvertera tillbaka till PowerShell-objekt. Här är ett exempel:

```PowerShell
$person = @{ name = "Johan"; age = 25 }
$json = $person | ConvertTo-Json
$json | ConvertFrom-Json
```

Resultatet från detta exempel blir ett PowerShell-objekt med namnet "Johan" och åldern 25. Notera att `@{}` används för att skapa ett hashtabell-objekt i PowerShell.

## Djupdykning:
JSON (JavaScript Object Notation) har sitt ursprung inom webbutveckling och är ett populärt sätt att strukturera data för att skicka mellan klient och server. Andra alternativ för att arbeta med data är till exempel XML och YAML. Det finns även möjlighet att använda externa moduler för mer avancerade operations.

## Se även:
För mer information om att arbeta med JSON i PowerShell, kan du läsa dokumentationen för `ConvertTo-Json` och `ConvertFrom-Json` cmdletar här: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/convertto-json?view=powershell-7. Inkludera även referenser till andra relevanta källor som JSON-specifikationen och populära externa moduler för arbete med JSON i PowerShell.