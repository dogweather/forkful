---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Arduino: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å konvertere en streng til små bokstaver innebærer å endre alle store bokstaver i en streng til deres lille bokstav tilsvarende. Vi gjør det for å unngå uønskede resultater ved sammenligning, fordi "Code" og "code" blir behandlet forskjellig i de fleste programmeringsspråk.

## Hvordan gjør man det:

For å konvertere en streng til små bokstaver i PowerShell, bruker vi .ToLower() metoden. Her er hvordan:

```PowerShell
$text = "Hei Verden!"
$lowerText = $text.ToLower()
Write-Host $lowerText
```
Når du kjører koden ovenfor, vil utdata være "hei verden!" - som er den opprinnelige strengen, men i små bokstaver.

## Dypere forståelse:

Historisk sett har konvertering til små bokstaver vært en vanlig funksjon i programmeringsspråk, med varierende implementeringer. I Bash, for eksempel, bruker du `tr '[:upper:]' '[:lower:]'`, mens i Python, det likner PowerShell med `str.lower()`.

Det finnes alternativer til .ToLower() i PowerShell. For eksempel kan du bruke -lowercase parameter med CaseConversion cmdlet fra PowerShell Community Extensions (Pscx) modulen:

```PowerShell
Import-Module Pscx
$text = "Hei Verden!"
$lowerText = CaseConversion -lower $text
Write-Host $lowerText
```

Når det gjelder implementeringsdetaljer, bruker .ToLower() -metoden Unicode-standarder for å matche og erstatte store bokstaver med deres motstykke i små bokstaver.

## Se også:

1. PowerShell Community Extensions (Pscx) - https://github.com/Pscx/Pscx
2. .NET String.ToLower Metode - https://docs.microsoft.com/dotnet/api/system.string.tolower
3. Sebastiaan's PowerShell Blog - https://www.powershellmagazine.com/author/sebastiaan-