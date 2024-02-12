---
title:                "Een datum converteren naar een string"
aliases: - /nl/powershell/converting-a-date-into-a-string.md
date:                  2024-01-28T21:57:44.093789-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum converteren naar een string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/powershell/converting-a-date-into-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een datum naar een string converteren in PowerShell betekent het veranderen van een `DateTime` object naar een tekstformaat. Programmeurs doen dit om datums op te maken voor weergave, logboeken, bestandsnamen of om informatie te serialiseren voor opslag en gegevensoverdracht.

## Hoe te:

Om een datum naar een string om te zetten, gebruiken we de `ToString` methode of de `-f` opmaakoperator. Hier is hoe:

```PowerShell
# Huidige datum en tijd
$date = Get-Date

# Standaardconversie naar string
$dateString = $date.ToString()
Write-Output $dateString

# Aangepast formaat: Jaar-Maand-Dag Uren:Minuten
$customFormat = $date.ToString("yyyy-MM-dd HH:mm")
Write-Output $customFormat

# Gebruik van -f operator voor hetzelfde aangepaste formaat
$fString = "{0:yyyy-MM-dd HH:mm}" -f $date
Write-Output $fString
```

Voorbeelduitvoer:

```
2023-03-17 10:45:00
2023-03-17 10:45
2023-03-17 10:45
```

## Diepgaand

PowerShell, geïnspireerd door Unix shells en Windows Script Host, introduceerde `Get-Date` in zijn vroege ontwikkeling rond 2006. Dit werd de go-to opdracht voor datum-tijd operaties. De `ToString` methode op `DateTime` objecten en de `-f` opmaakoperator zijn geleende concepten van .NET, waardoor PowerShell zijn objectgeoriënteerde smaken kreeg.

Als `ToString()` niet is gespecificeerd met een formaat, geeft het de volledige datum en tijd weer in het formaat van de huidige cultuur. Maar wanneer je een specifieke lay-out nodig hebt, zoals ISO 8601 of gewoon de dag en maand, worden aangepaste .NET datum- en tijdformaatstrings je vrienden.

Er is nog een ouderwetse manier—gebruik maken van de `DateTime` formaatpatronen zoals `yyyy` voor een viercijferig jaar, `MM` voor een nul-gevulde maand. Ze zijn intuïtief en talrijk voor het vormgeven van elk datum-tijd formaat.

Dan is er POSIX in Unix, waar `date` commando's regeren met hun eigen opmaakspecificaties. PowerShell overbrugde de twee werelden, door bekende methoden aan te nemen maar ook zware compatibiliteit met Windows systemen te bieden.

Alternatieven omvatten basisconcatenatie van datumcomponenten en het gebruik van externe hulpprogramma's of taalstructuren. PowerShell geeft echter de voorkeur om zaken in eigen huis te houden met robuuste native commando's.

Je kunt dieper duiken in opmaakspecificaties in de officiële Microsoft-documentatie of community-geschreven blogs ontdekken die vaak creatieve manieren delen om datums en tijden in PowerShell te manipuleren.

## Zie Ook

- De officiële documentatie van PowerShell over de [Get-Date](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/get-date) cmdlet, die gebruik en voorbeelden biedt.
- De gids van .NET voor standaard en aangepaste [formaatstrings](https://docs.microsoft.com/dotnet/standard/base-types/standard-date-and-time-format-strings) voor diepgaande formatteringsdetails.
- Communityblogs zoals de [PowerShell.org forums](https://powershell.org/forums/) of [Stack Overflow](https://stackoverflow.com/questions/tagged/powershell+datetime) voor voorbeelden uit de praktijk en probleemoplossende discussies.
