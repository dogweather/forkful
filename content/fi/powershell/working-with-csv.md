---
title:                "CSV-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Bash: CSV-tiedostojen käsittely"
simple_title:         "CSV-tiedostojen käsittely"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä ja miksi?
CSV on yksinkertainen tiedostomuoto, joka sisältää taulukkomaisia tietoja tekstimuodossa. Ohjelmoijat käyttävät CSV:tä, koska se on helppolukuinen ja sitä tukevat monet ohjelmistot ja kielet, mikä tekee tietojen siirrosta vaivatonta.

## How to - Kuinka tehdä:
```PowerShell
# CSV-tiedoston lukeminen:
$csvData = Import-Csv -Path "C:\data\esimerkki.csv"

# CSV-tiedoston kirjoittaminen:
$people = @(
    [PSCustomObject]@{Nimi='Maija'; Ikä=30; Kaupunki='Helsinki'},
    [PSCustomObject]@{Nimi='Kalle'; Ikä=42; Kaupunki='Espoo'}
)
$people | Export-Csv -Path "C:\data\uusi_esimerkki.csv" -NoTypeInformation

# Tulostetaan `$csvData`
$csvData
```
Sample output:
```
Nimi  Ikä Kaupunki
----  --- --------
Maija 30  Helsinki
Kalle 42  Espoo
```

## Deep Dive - Syvempi katsaus:
CSV (Comma-Separated Values) on peräisin 1970-luvulta ja oli alun perin tarkoitettu yksinkertaiseen, sarakepohjaiseen datan tallentamiseen. Nykyään on olemassa XML ja JSON, jotka ovat monimutkaisempia mutta myös tehokkaampia vaihtoehtoja. PowerShell käyttää .NET-luokkia kuten `TextFieldParser` tai `OleDb` CSV:n käsittelyyn, mutta `Import-Csv` ja `Export-Csv` ovat yksinkertaisia ja tehokkaita cmdlet-komentoja näihin tarpeisiin.

## See Also - Katso myös:
- PowerShell-dokumentaatio `Import-Csv`: https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/import-csv
- PowerShell-dokumentaatio `Export-Csv`: https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/export-csv
- CSV-tiedostot Wiki: https://fi.wikipedia.org/wiki/CSV-tiedosto
- Microsoft Excel -tuki CSV-tiedostoille: https://support.microsoft.com/fi-fi/office/csv-tiedostomuodon-tuominen-ja-viennin-ohjeet-3e1d9630-8cc4-4cd2-af6b-5c725ee173c8
