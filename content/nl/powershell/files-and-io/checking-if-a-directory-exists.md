---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:39.889899-07:00
description: "Controleren of een directory bestaat is simpelweg verifi\xEBren of een\
  \ map aanwezig is op een gespecificeerd pad in het bestandssysteem. Programmeurs\
  \ doen\u2026"
lastmod: '2024-02-25T18:49:48.378853-07:00'
model: gpt-4-0125-preview
summary: "Controleren of een directory bestaat is simpelweg verifi\xEBren of een map\
  \ aanwezig is op een gespecificeerd pad in het bestandssysteem. Programmeurs doen\u2026"
title: Controleren of een directory bestaat
---

{{< edit_this_page >}}

## Wat & Waarom?

Controleren of een directory bestaat is simpelweg verifiëren of een map aanwezig is op een gespecificeerd pad in het bestandssysteem. Programmeurs doen dit om fouten te voorkomen, bestanden efficiënt te beheren en te zorgen dat gegevens van of naar de juiste locaties worden geschreven of gelezen.

## Hoe:

Gebruik de `Test-Path` cmdlet om te controleren op het bestaan van een directory. Deze cmdlet retourneert een boolean: `$true` als de directory bestaat, en `$false` als dat niet zo is.

```PowerShell
# Controleren of een directory bestaat
$directoryPath = "C:\VoorbeeldMap"
$exists = Test-Path $directoryPath
Write-Output $exists  # Geeft True of False terug
```

Voorbeelduitvoer:
```
True
```
of als de directory niet bestaat:
```
False
```

Je kunt het ook direct gebruiken in een `if`-instructie:

```PowerShell
# Test-Path gebruiken in een if-instructie
if (Test-Path $directoryPath) {
    Write-Output "Ja, het is er."
} else {
    Write-Output "Nee, kan het niet vinden."
}
```

## Diepere Duik

De `Test-Path` cmdlet is er al sinds PowerShell v1.0. Het is geen eendagsvlieg; naast directories kan het ook gebruikt worden voor het controleren van bestanden, registervermeldingen, en andere items via verschillende 'paden'.

Er zijn alternatieven. PowerShell is gebouwd op het .NET Framework, dus je zou kunnen overstappen op .NET-methoden als je dat wilt:

```PowerShell
[system.io.directory]::Exists($directoryPath)
```

Dit dient hetzelfde doel maar is de "lange route". Waarom deze moeite, wanneer `Test-Path` speciaal voor deze taak is gemaakt?

Wat implementatie betreft, is het controleren op een directory voordat operaties worden uitgevoerd best practice. Het gaat om voorspelbaarheid. Je zou toch ook geen dragrace beginnen met een lege tank? Dus je leest niet van of schrijft niet naar een niet-bestaande directory, evenzeer.

## Zie Ook

Voor meer informatie, bekijk deze links:

- [Test-Path Cmdlet Documentatie](https://docs.microsoft.com/nl-nl/powershell/module/microsoft.powershell.management/test-path)
- [.NET Directory.Exists Methode](https://docs.microsoft.com/nl-nl/dotnet/api/system.io.directory.exists)
