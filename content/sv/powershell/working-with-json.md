---
title:                "Arbeta med JSON"
date:                  2024-01-19
simple_title:         "Arbeta med JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?
JSON (JavaScript Object Notation) är ett lättviktigt datautbytesformat. Programmerare använder det för att lagra och överföra data, på grund av dess läsbarhet och enkla integration med webbteknologier.

## How to:
```PowerShell
# Skapa ett objekt och konvertera till JSON
$person = @{
    Namn = 'Anna'
    Ålder = 30
    Stad = 'Stockholm'
}

# Konvertera till JSON
$json = $person | ConvertTo-Json
Write-Output $json

# Output:
#{
#    "Namn":  "Anna",
#    "Ålder":  30,
#    "Stad":  "Stockholm"
#}

# Läs in JSON från en fil
$jsonString = Get-Content -Path 'person.json' -Raw

# Konvertera JSON-sträng till PowerShell-objekt
$personObject = $jsonString | ConvertFrom-Json
Write-Output $personObject

# Output:
#Namn    Ålder Stad     
#----    ----- ----     
#Anna    30    Stockholm
```

## Deep Dive
JSON lanserades tidigt 2000-tal och designades för att vara ett enkelt format för att serialisera komplexa datastrukturer. XML var tidigare det dominant val men JSON tog över p.g.a sin mindre verbositet. Alternativ för att hantera JSON i PowerShell är bland annat `ConvertTo-Json` och `ConvertFrom-Json`. Mer detaljerade anpassningar inkluderar `-Depth`-parameter för att hantera djupa objekt och `-Compress` för minifierad JSON.

## See Also
- [JSON.org – JSON introduction](https://www.json.org/json-en.html)
- [Wikipedia – JSON](https://sv.wikipedia.org/wiki/JSON)
