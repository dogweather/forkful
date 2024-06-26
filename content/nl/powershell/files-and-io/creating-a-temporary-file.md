---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:20.963329-07:00
description: "Hoe: Om in PowerShell een tijdelijk bestand te maken, gebruik je `New-TemporaryFile`.\
  \ Deze cmdlet maakt een tijdelijk bestand in je temp-map. Hier is de\u2026"
lastmod: '2024-03-13T22:44:51.049513-06:00'
model: gpt-4-0125-preview
summary: Om in PowerShell een tijdelijk bestand te maken, gebruik je `New-TemporaryFile`.
title: Een tijdelijk bestand aanmaken
weight: 21
---

## Hoe:
Om in PowerShell een tijdelijk bestand te maken, gebruik je `New-TemporaryFile`. Deze cmdlet maakt een tijdelijk bestand in je temp-map. Hier is de toverspreuk:

```PowerShell
$tempFile = New-TemporaryFile
```

Deze regel roept een gloednieuw tijdelijk bestand op uit de digitale ether. Wil je weten waar het zich bevindt? Typ gewoon:

```PowerShell
$tempFile.FullName
```

En bam! Het vertelt je het pad van het bestand. Wanneer je klaar bent en het wilt opruimen, verwijder je het gewoon:

```PowerShell
Remove-Item $tempFile.FullName
```

Het bestand verdwijnt zonder sporen na te laten.

## Diepere duik
Nu, laten we onder de motorkap kijken. Historisch gezien worden temp-bestanden gebruikt sinds de dageraad van de computerwereld, voornamelijk omdat RAM schaars en kostbaar was. Deze overgangsbestanden waren een oplossing voor beperkt geheugen.

Wat betreft alternatieven, sommige ontwikkelaars maken handmatig hun tijdelijke bestandspaden met `[System.IO.Path]::GetTempFileName()`, wat werkt in verschillende door .NET ondersteunde talen en geeft je meer controle.

In PowerShell is `New-TemporaryFile` eigenlijk een stijlvolle wrapper rond deze .NET-methode. Het creëert een bestand op een pad zoals `C:\Users\JouwNaam\AppData\Local\Temp\tmpXXXX.tmp` (`XXXX` is een willekeurig getal). De extensie `.tmp` is een conventie, die aangeeft dat het tijdelijk is.

Onthoud dat temp-bestanden naar behoren moeten worden verwijderd. Als je er veel creëert of gevoelige gegevens behandelt, moet je ze zorgvuldig wissen om gegevenslekken te voorkomen.

## Zie ook
- Voor meer over `New-TemporaryFile`, controleer de [docs](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/new-temporaryfile).
- Duik in de methoden van de klasse `System.IO.Path` op [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.io.path?view=net-6.0).
