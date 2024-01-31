---
title:                "Werken met complexe getallen"
date:                  2024-01-28T22:12:29.314281-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met complexe getallen"

category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/powershell/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Complexe getallen, die een reëel deel en een imaginair deel hebben (zoals 3 + 4i), zijn essentieel in vakgebieden zoals techniek, natuurkunde en datawetenschap. Programmeurs gebruiken ze voor simulaties, signaalverwerking en het oplossen van specifieke soorten wiskundige problemen.

## Hoe te:
PowerShell heeft geen ingebouwde ondersteuning voor complexe getallen, dus je moet ofwel je eigen oplossing ontwikkelen of .NET's `System.Numerics.Complex` gebruiken.

```PowerShell
# Laten we complexe getallen maken met .NET
[Reflection.Assembly]::LoadWithPartialName("System.Numerics") | Out-Null

# Maak complexe getallen
$complex1 = [System.Numerics.Complex]::new(3, 4) # 3 + 4i
$complex2 = [System.Numerics.Complex]::new(1, 2) # 1 + 2i

# Voeg twee complexe getallen samen
$sum = [System.Numerics.Complex]::Add($complex1, $complex2) # 4 + 6i

# Vermenigvuldig twee complexe getallen
$product = [System.Numerics.Complex]::Multiply($complex1, $complex2) # -5 + 10i

# Toon de resultaten
"Som: $sum"
"Product: $product"
```
Output:
```
Som: (4, 6)
Product: (-5, 10)
```

## Diepgaande Duik
Complexe getallen werden ontwikkeld in de 16e eeuw om vergelijkingen op te lossen die geen oplossingen hadden in het domein van de reële getallen. Ze zijn nu een hoeksteen van de moderne wiskunde.

De afhankelijkheid van PowerShell op .NET voor ondersteuning van complexe getallen betekent dat de prestaties solide zijn. Alternatieven zijn onder meer bibliotheken van derden of andere programmeertalen zoals Python, waar complexe getallen een native gegevenstype zijn.

## Zie Ook
- [System.Numerics.Complex Structuur](https://docs.microsoft.com/nl-nl/dotnet/api/system.numerics.complex)
- [Rekenkunde met Complexe Getallen in Python](https://docs.python.org/3/library/cmath.html)
