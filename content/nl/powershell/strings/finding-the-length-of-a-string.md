---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:15.674976-07:00
description: 'Hoe te: PowerShell maakt het verkrijgen van de tekenreekslengte eenvoudig.
  Gooi gewoon een tekenreeks naar de `.Length` eigenschap, zoals dit.'
lastmod: '2024-03-13T22:44:51.017431-06:00'
model: gpt-4-0125-preview
summary: PowerShell maakt het verkrijgen van de tekenreekslengte eenvoudig.
title: De lengte van een string vinden
weight: 7
---

## Hoe te:
PowerShell maakt het verkrijgen van de tekenreekslengte eenvoudig. Gooi gewoon een tekenreeks naar de `.Length` eigenschap, zoals dit:

```PowerShell
$myString = "Hallo, Wereld!"
$myStringLength = $myString.Length
Write-Host "De tekenreekslengte is: $myStringLength"
```

Je krijgt de uitvoer:

```
De tekenreekslengte is: 13
```

Dat is alles wat erbij komt kijken. Direct en pijnloos.

## Diepere Duik
Vroeger betekende het verkrijgen van de lengte van een tekenreeks in de meeste programmeertalen complexe functies of processen. Vandaag de dag is het zo simpel als een eigenschapsaanroep in PowerShell.

Voorbij de basis `.Length` eigenschap, biedt PowerShell geen ingebouwde alternatieven voor deze specifieke taak. Echter, voordat PowerShell een ding werd, werd scripting in Windows gedaan via batchbestanden of VBScript, waar het vinden van een tekenreekslengte niet zo eenvoudig was.

Wat betreft de implementatie, wanneer je `$myString.Length` gebruikt, heeft PowerShell toegang tot de metadata van het tekenreeksobject – tekenreeksen in PowerShell zijn objecten van de System.String klasse, die uit .NET komt. De `.Length` eigenschap is een lid van die klasse.

## Zie Ook
Duik dieper in PowerShell tekenreeksen:

Voor bredere context over hoe tekenreeksen werken in .NET:
- [String Klasse in .NET](https://docs.microsoft.com/dotnet/api/system.string)
- [String.Length Eigenschap in .NET](https://docs.microsoft.com/dotnet/api/system.string.length)
