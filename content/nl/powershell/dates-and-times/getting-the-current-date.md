---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:29.907974-07:00
description: "De huidige datum ophalen in PowerShell is simpelweg het systeemidee\
  \ van de datum van vandaag verkrijgen. Programmeurs gebruiken dit om logboeken te\u2026"
lastmod: '2024-03-13T22:44:51.040433-06:00'
model: gpt-4-0125-preview
summary: De huidige datum ophalen in PowerShell is simpelweg het systeemidee van de
  datum van vandaag verkrijgen.
title: Het huidige datum ophalen
weight: 29
---

## Hoe:
Hier is de directe code om de datum van vandaag te pakken:

```PowerShell
Get-Date
```

En voila, output:

```plaintext
Dinsdag 14 maart 2023 10:15:42
```

Misschien wil je iets specifiekers, zoals alleen de dag:

```PowerShell
(Get-Date).Day
```
Output:

```plaintext
14
```

Hoe zit het met internationaal gaan? Krijg de datum in ISO 8601 formaat:

```PowerShell
Get-Date -Format 'yyyy-MM-dd'
```

Output:

```plaintext
2023-03-14
```

## Diepere Duik
Vroeger was het krijgen van de datum in scripttalen geen eenvoudige zaak. Maar PowerShell, door te leren van de complexiteiten en noodzakelijkheden van de computergeschiedenis, maakte het een eenregelige taak.

Naast `Get-Date`, omvatten alternatieven het duiken in de .NET System.DateTime klasse voor meer complexe behoeften, of het gebruik van WMI (Windows Management Instrumentation) om systeeminformatie op te halen. Toch is `Get-Date` de eerste keuze voor eenvoud en effectiviteit.

Onder de motorkap maakt `Get-Date` gebruik van de klok en regionale instellingen van je systeem om ervoor te zorgen dat de datum en tijd nauwkeurig worden weergegeven, rekening houdend met tijdzones en aanpassingen voor zomertijd.

Het is ook volledig aanpasbaar. Je kunt het uitvoerformaat naar wens vormgeven met behulp van standaard of aangepaste formatstrings - een handige functie voor logboeken die bepaalde conventies moeten volgen of voor visuele harmonie in je uitvoer.

## Zie Ook
Hier zijn een paar bronnen om te bekijken:

- [Get-Date documentatie](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [.NET DateTime structuur](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [Aangepaste datum- en tijdformatstrings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
