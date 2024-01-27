---
title:                "Arbeid med CSV"
date:                  2024-01-19
html_title:           "Bash: Arbeid med CSV"
simple_title:         "Arbeid med CSV"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
CSV (Comma Separated Values) er et enkelt filformat som brukes til å lagre tabelldata, slik som en database eller et regneark i ren tekst. Programmerere bruker CSV fordi det er lett å lese og skrive data i et standard format som kan deles mellom ulike systemer.

## Hvordan gjøre det:
For å jobbe med CSV-filer i PowerShell bruker vi ofte Import-Csv og Export-Csv cmdletene. La oss se noen eksempler:

```PowerShell
# Importere en CSV-fil
$data = Import-Csv -Path "C:\eksempel\data.csv"

# Vise de første fem radene
$data | Select-Object -First 5

# Eksportere data til en ny CSV-fil
$data | Export-Csv -Path "C:\eksempel\utdata.csv" -NoTypeInformation
```

Sample output for å vise de første fem radene:

```PowerShell
Navn  Alder  Yrke
----  -----  ----
Ola   34     Snekr
Kari  28     Designer
Per   45     Lærer
Eva   52     Forsker
Jon   38     Utvikler
```

## Dypdykk
Historisk sett kommer CSV fra tidlige dager i databehandling hvor det var et behov for et enkelt, tekstbasert format for å utveksle data. Alternativene inkluderer nå XML, JSON og andre spesialiserte dataformater, men CSV forblir populær på grunn av sin enkelhet.

Implementasjonsdetaljer å merke seg i PowerShell er at Import-Csv automatisk lager objekter hvor hvert felt i CSV blir en egenskap. Dette gjør det enkelt å manipulere dataene med andre PowerShell cmdlets.

## Se Også
- Microsofts offisielle dokumentasjon for Import-Csv: https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/import-csv
- Microsofts offisielle dokumentasjon for Export-Csv: https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/export-csv
- Learn PowerShell Book - Arbeide med CSV-filer: https://www.leanpub.com/learn-powershell
