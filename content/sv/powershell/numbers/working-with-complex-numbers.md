---
date: 2024-01-26 04:44:09.636982-07:00
description: "Komplexa tal, de med en reell del och en imagin\xE4r del (som 3 + 4i),\
  \ \xE4r avg\xF6rande inom omr\xE5den som ingenj\xF6rsvetenskap, fysik och data vetenskap.\u2026"
lastmod: '2024-02-25T18:49:36.433256-07:00'
model: gpt-4-0125-preview
summary: "Komplexa tal, de med en reell del och en imagin\xE4r del (som 3 + 4i), \xE4\
  r avg\xF6rande inom omr\xE5den som ingenj\xF6rsvetenskap, fysik och data vetenskap.\u2026"
title: Att arbeta med komplexa tal
---

{{< edit_this_page >}}

## Vad & Varför?
Komplexa tal, de med en reell del och en imaginär del (som 3 + 4i), är avgörande inom områden som ingenjörsvetenskap, fysik och data vetenskap. Programmerare använder dem för simuleringar, signalbehandling och för att lösa specifika typer av matematiska problem.

## Hur:
PowerShell har inte inbyggt stöd för komplexa tal, så du måste antingen skapa din egen lösning eller använda .NET's `System.Numerics.Complex`.

```PowerShell
# Låt oss skapa komplexa tal med .NET
[Reflection.Assembly]::LoadWithPartialName("System.Numerics") | Out-Null

# Skapa komplexa tal
$complex1 = [System.Numerics.Complex]::new(3, 4) # 3 + 4i
$complex2 = [System.Numerics.Complex]::new(1, 2) # 1 + 2i

# Addera två komplexa tal
$sum = [System.Numerics.Complex]::Add($complex1, $complex2) # 4 + 6i

# Multiplicera två komplexa tal
$product = [System.Numerics.Complex]::Multiply($complex1, $complex2) # -5 + 10i

# Visa resultaten
"Summa: $sum"
"Produkt: $product"
```
Utskrift:
```
Summa: (4, 6)
Produkt: (-5, 10)
```

## Fördjupning
Komplexa tal utvecklades under 1500-talet för att lösa ekvationer som inte hade lösningar inom området för reella tal. De är nu en hörnsten i den moderna matematiken.

PowerShell's beroende av .NET för stöd till komplexa tal innebär att prestandan är solid. Alternativ inkluderar tredjepartsbibliotek eller andra programmeringsspråk som Python, där komplexa tal är en inbyggd datatyp.

## Se även
- [System.Numerics.Complex Struktur](https://docs.microsoft.com/en-us/dotnet/api/system.numerics.complex)
- [Komplexa talens aritmetik i Python](https://docs.python.org/3/library/cmath.html)
