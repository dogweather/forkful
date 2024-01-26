---
title:                "Datum aus einem String parsen"
date:                  2024-01-20T15:37:46.834986-07:00
html_title:           "Arduino: Datum aus einem String parsen"
simple_title:         "Datum aus einem String parsen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen eines Datums aus einem String bedeutet, Text in ein Datum-Objekt umzuwandeln. Programmierer machen das, um mit Daten zu arbeiten, die in menschenlesbarem Format eingeben oder empfangen werden.

## How to:
Hier ein paar Beispiele, wie man das anstellt:

```PowerShell
# Parsen eines einfachen Datumsformats
$datumString = "31.12.2023"
$datumObjekt = [DateTime]::ParseExact($datumString, "dd.MM.yyyy", $null)
$datumObjekt

# Parsen mit Kulturinformationen (Deutsches Format)
$deKultur = [Globalization.CultureInfo]::GetCultureInfo("de-DE")
$datumObjektDe = [DateTime]::ParseExact($datumString, "dd.MM.yyyy", $deKultur)
$datumObjektDe
```

Ausgabe:
```
Sonntag, 31. Dezember 2023 00:00:00
Sonntag, 31. Dezember 2023 00:00:00
```

## Deep Dive:
Das Umwandeln von Strings in Datumsangaben ist nichts Neues. Früher war es komplizierter und fehleranfälliger. Heutzutage bieten Programmiersprachen wie PowerShell integrierte Funktionen dafür. Es gibt Alternativen zum ParseExact, z.B. `Parse`, `TryParse`, und `TryParseExact`, die unterschiedlich rigide mit dem Format umgehen. Wichtig sind auch Kulturinformationen (`CultureInfo`), da Datumsformate weltweit variieren. Ohne spezifische Kulturangaben nutzt PowerShell die Kultur des Betriebssystems.

## See Also:
- [CultureInfo Class - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=net-6.0)
- [DateTime.ParseExact Method - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parseexact?view=net-6.0)
- [DateTime.TryParseExact Method - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tryparseexact?view=net-6.0)
