---
title:                "Arbeta med csv"
date:                  2024-01-19
html_title:           "Arduino: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
CSV (Comma-Separated Values) används för att lagra data i enkel textform, uppskattad för sin enkelhet och bredd av kompatibilitet. Programmerare använder CSV-formatet för att enkelt importera, exportera och manipulera data mellan program och system.

## How to:
Så här jobbar du med CSV-filer i PowerShell.

Importera en CSV-fil:
```PowerShell
$csvData = Import-Csv -Path "C:\exempel.csv"
```

Visa innehållet:
```PowerShell
$csvData
```

Skapa en ny CSV-fil:
```PowerShell
$nyData = [PSCustomObject]@{Namn="Anna"; Ålder=28; Stad="Stockholm"}
$nyData | Export-Csv -Path "C:\ny_exempel.csv" -NoTypeInformation
```

Lägg till data i befintlig CSV-fil:
```PowerShell
$extraData = [PSCustomObject]@{Namn="Björn"; Ålder=35; Stad="Göteborg"}
$extraData | Export-Csv -Path "C:\exempel.csv" -Append -NoTypeInformation
```

Filtrera data:
```PowerShell
$äldreAn30 = $csvData | Where-Object {$_.'Ålder' -gt 30}
$äldreAn30
```

## Deep Dive
CSV introducerades under tidigt 1970-tal. Formatet har blivit standard för datautbyte tack vare dess textbaserade natur vilket gör det platformsoberoende. Alternativ till CSV inkluderar JSON och XML, som båda bär på mer datastruktur och -beskrivningar men kräver mer overhead. PowerShell använder cmdlets såsom `Import-Csv` och `Export-Csv` för att interagera med CSV, vilket eliminerar behovet av manuell textparsning.

## See Also
- Mer om alternativa dataformat: JSON – [https://www.json.org/json-en.html](https://www.json.org/json-en.html), XML – [https://www.w3.org/XML/](https://www.w3.org/XML/)
- En djupdykning i cmdlets för CSV: [https://ss64.com/ps/](https://ss64.com/ps/)
