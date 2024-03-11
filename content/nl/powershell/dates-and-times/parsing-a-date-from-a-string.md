---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:49.555678-07:00
description: "Het parsen van een datum uit een string gaat over het begrijpelijk maken\
  \ van datuminformatie die binnen een tekst is opgenomen. Programmeurs doen dit om\u2026"
lastmod: '2024-03-11T00:14:24.873611-06:00'
model: gpt-4-0125-preview
summary: "Het parsen van een datum uit een string gaat over het begrijpelijk maken\
  \ van datuminformatie die binnen een tekst is opgenomen. Programmeurs doen dit om\u2026"
title: Een datum uit een string parsen
---

{{< edit_this_page >}}

## Wat & Waarom?
Het parsen van een datum uit een string gaat over het begrijpelijk maken van datuminformatie die binnen een tekst is opgenomen. Programmeurs doen dit om berekeningen, vergelijkingen mogelijk te maken, en datums in een gestandaardiseerd formaat op te slaan.

## Hoe te:
PowerShell maakt het parsen van datums vrij eenvoudig. Laten we eens kijken hoe we een string kunnen omzetten in een DateTime-object.

```PowerShell
# Basis parsing met ParseExact
$dateString = "March 31, 2023"
$format = "MMMM dd, yyyy"
$parsedDate = [datetime]::ParseExact($dateString, $format, $null)

# Uitvoer
$parsedDate
```

Dit zal uitvoeren:

```
Vrijdag, 31 maart 2023 12:00:00 AM
```

Soms hebben strings verschillende formaten. Laten we dat aanpakken met `Parse` standaard:

```PowerShell
# Een datum met verschillende formaten parsen
$dateString = "2023-03-31T14:45:00"
$parsedDate = [datetime]::Parse($dateString)

# Uitvoer
$parsedDate
```

De uitvoer hier:

```
Vrijdag, 31 maart 2023 14:45:00 PM
```

Omgaan met cultuurspecifieke formaten? Gebruik `ParseExact` met een specifieke cultuur:

```PowerShell
# Cultuurspecifiek parsen
$dateString = "31/03/2023 16:45"
$format = "dd/MM/yyyy HH:mm"
$culture = [Globalization.CultureInfo]::GetCultureInfo("en-GB")
$parsedDate = [datetime]::ParseExact($dateString, $format, $culture)

# Uitvoer
$parsedDate
```

Die geeft als uitvoer:

```
Vrijdag, 31 maart 2023 16:45:00 PM
```

## Diepgaande Duik
Nu voor wat ingewikkeldere details. Datumsparsing is altijd lastig geweest vanwege verschillende wereldwijde datumformaten. Vroeger had elke programmeertaal zijn eigen manier om met datums om te gaan, wat vaak tot inconsistenties leidde. PowerShell, als een nieuwere scripttaal, profiteerde van de DateTime-klasse van .NET, die robuuste parsingfuncties biedt.

Als alternatieven hebben we `Get-Date`, dat ook een string in een DateTime-object kan parsen. Overweeg `TryParseExact` en `TryParse` als u het onverwachte verwacht en uitzonderingen van onparsbare strings wilt vermijden.

Laten we het hebben over implementatie. Datumsparsing is cultuurgevoelig, wat betekent dat het datumformaat per regio kan variëren. Daarom geven we cultuur- en formaatinformatie om de verwachte structuur van de datumstring vast te leggen.

Soms kom je zeer eigenaardige formaten tegen - hier komt de `ParseExact` methode van pas, je Zwitsers zakmes voor datumsparsing. Het stelt je in staat om het exacte formaat op te geven dat je verwacht. Geen gokspelletjes hier.

## Zie ook
Om je kennis uit te breiden, duik in deze bronnen:

- [Officiële PowerShell-documentatie over Get-Date](https://docs.microsoft.com/nl-nl/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [.NET-documentatie over DateTime.Parse](https://docs.microsoft.com/nl-nl/dotnet/api/system.datetime.parse?view=net-6.0)
- [Overwegingen omtrent globalisering en lokalisatie](https://docs.microsoft.com/nl-nl/dotnet/standard/globalization-localization/)

Onthoud, datumsparsing is krachtig, dus hanteer het verstandig!
