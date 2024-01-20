---
title:                "Å jobbe med csv"
html_title:           "PowerShell: Å jobbe med csv"
simple_title:         "Å jobbe med csv"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?
Når du jobber med data, er CSV et nyttig format å kjenne til. Det står for "Comma Separated Values" og er en måte å lagre og transportere tabellformatert data på. Dette er spesielt nyttig for programmerere, da det gjør det enklere å behandle store mengder data på en strukturert måte.

## Hvordan:
Her er et enkelt eksempel på hvordan du kan jobbe med CSV-filer i PowerShell:

```PowerShell
#Importer CSV-fil
$csv = Import-Csv -Path "C:\Eksempel.csv"

#Velg bestemte kolonner
$csv | Select-Object "Navn", "Alder"

#Lag en ny CSV-fil
$csv | Export-Csv -Path "C:\Ny.csv" -NoTypeInformation
```

Dette vil importere en CSV-fil, velge spesifikke kolonner og eksportere dem til en ny fil. Her er et eksempel på utdata:

```PowerShell
Navn        Alder
----        -----
Per         34
Mia         28
Jonas       42
```

## Dypdykk:
CSV er et format som har eksistert i mange år, og er fortsatt utbredt i dag på grunn av sin enkelhet og kompatibilitet med ulike systemer og programmeringsspråk. Alternativene for å jobbe med CSV på PowerShell inkluderer også `ConvertFrom-Csv` og `ConvertTo-Csv` kommandoer.

Når du jobber med CSV-filer i PowerShell, er det viktig å huske på at kolonnedataene blir importert som objekter, og ikke som ren tekst. Dette betyr at du kan bruke objektmetoder som `Select-Object` og `Where-Object` for å bearbeide dataene.

## Se også:
- [PowerShell dokumentasjon for Import-Csv](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/import-csv)
- [Wikipedia artikkel om CSV formatet](https://en.wikipedia.org/wiki/Comma-separated_values)