---
title:                "Omvandla ett datum till en sträng"
html_title:           "C#: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att konvertera ett datum till en sträng innebär att ändra datats format till en textrepresentation, exempelvis "2022-02-24". Programmörer gör det för att göra datan mer läsbar och lätt att manipulera.

## Hur fungerar det:

För att konvertera datum till sträng i PowerShell, kan man använda `Get-Date` kommandot för att få aktuell tid och datum, och sedan använda `-Format` för att ange formatet. Här är ett exempel:

```PowerShell
# Hämta nuvarande datum
$datum = Get-Date
Write-Host "Originaldatum: $datum "

# Konvertera till sträng
$strängDatum = $datum.ToString('yyyy-MM-dd')
Write-Host "Sträng Representation: $strängDatum"
```

Om du kör den här koden, borde du se något liknande som output:

```PowerShell
Originaldatum: 2022-02-24 9:37:20
Sträng Representation: 2022-02-24
```
ekvensen 'yyyy-MM-dd' definierar output formatet.

## Djupgående:

+ Historisk Kontext: PowerShell har alltid haft behovet att konvertera datum till strängar för att göra det mer läsbart för användare eller för att passa specifika programmeringskrav.

+ Alternativ: Du kan också använda metoden `Get-Format`, men `ToString()` är mycket mer lättanvänd och konsekvent.

+ Implementeringsdetaljer: När du kallar på `ToString('yyyy-MM-dd')`, ber du faktiskt .NET miljön att konvertera ditt datum till en sträng. Det är bra att veta om du försöker felsöka.

## Se Även:

+ [`Get-Date` dokumentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7)
+ [Konvertera datum och tid till sträng](https://stackoverflow.com/questions/3141412/standard-date-and-time-format-strings)
+ [Microsoft PowerShell dokumentation](https://docs.microsoft.com/en-us/powershell/)