---
date: 2024-01-20 17:48:00.096380-07:00
description: "S\xE5 h\xE4r g\xF6r du: H\xE4r \xE4r ett enkelt exempel p\xE5 hur du\
  \ hittar en str\xE4ngs l\xE4ngd i PowerShell."
lastmod: '2024-03-13T22:44:38.115187-06:00'
model: gpt-4-1106-preview
summary: "H\xE4r \xE4r ett enkelt exempel p\xE5 hur du hittar en str\xE4ngs l\xE4\
  ngd i PowerShell."
title: "Hitta l\xE4ngden p\xE5 en str\xE4ng"
weight: 7
---

## Så här gör du:
Här är ett enkelt exempel på hur du hittar en strängs längd i PowerShell:

```PowerShell
$sträng = "Hej Sverige!"
$längd = $sträng.Length
Write-Host "Längden på strängen är: $längd"
```

Exempelutdata:

```
Längden på strängen är: 12
```

## Fördjupning
Längden på en sträng är viktig sedan programmeringens gryning. I PowerShell används `.Length`-egenskapen för detta, som är en arv från .NET-objektets grunder. Alternativt kan man använda `Measure-Object` cmdlet, men det är onödigt för enkel längdberäkning. `.Length` är snabb, enkel och den direkta vägen till informationen du behöver.

```PowerShell
# Alternativ metod
$sträng | Measure-Object -Character | Select-Object -ExpandProperty Characters
```

Detaljerna i hur `.Length` implementeras ligger i hur strängar representeras i minnet – som en samling tecken. PowerShell hanterar strängar som objekt och varje objekt vet hur långt det är.

## Se även
- [PowerShell Documentation on Microsoft Docs](https://docs.microsoft.com/powershell/)
- [Understanding .NET String Object on Microsoft Docs](https://docs.microsoft.com/dotnet/api/system.string)
- [PowerShell `Measure-Object` documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/measure-object)
