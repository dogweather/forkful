---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:49.550942-07:00
description: 'Hoe: PowerShell is vrij handig met strings. Gebruik de `.ToLower()`
  methode, zoals dit.'
lastmod: '2024-03-13T22:44:51.013390-06:00'
model: gpt-4-0125-preview
summary: PowerShell is vrij handig met strings.
title: Een string omzetten naar kleine letters
weight: 4
---

## Hoe:
PowerShell is vrij handig met strings. Gebruik de `.ToLower()` methode, zoals dit:

```PowerShell
$string = "HELLO, World!"
$lowerCaseString = $string.ToLower()
$lowerCaseString
```

Uitvoer:

```
hello, world!
```

Of probeer de `ToLowerInvariant()` methode wanneer culturele normen de conversie niet zouden moeten beïnvloeden:

```PowerShell
$string = "HELLO, World!"
$lowerCaseInvariant = $string.ToLowerInvariant()
$lowerCaseInvariant
```

Uitvoer:

```
hello, world!
```

## Diepgaand
Er was een tijd dat hoofdletterongevoeligheid vrij gebruikelijk was in programmeertalen. In PowerShell, zoals zijn .NET voorgangers, zijn strings objecten met ingebouwde methoden voor manipulatie. Wanneer we `.ToLower()` gebruiken, roepen we een methode op die het conversieproces voor ons afhandelt.

Alternatieve manieren om het werk gedaan te krijgen? Zeker. Je zou kunnen gebruiken:

- een `for` lus, elk karakter bezoeken en handmatig van geval veranderen
- Reguliere Expressies met de `-replace` operator
- Cultuurspecifieke conversies met behulp van overloads van `.ToLower()`

Waarom de invariante cultuur gebruiken met `ToLowerInvariant()`? Het is essentieel voor consistente resultaten over verschillende locales waar de interpretatie van wat een "kleine" letter is, kan verschillen.

## Zie Ook
Voor meer gedetailleerde avonturen in stringmanipulatie, bezoek deze links:

- [.NET String Class](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=net-6.0)
