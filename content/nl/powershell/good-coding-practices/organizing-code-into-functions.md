---
title:                "Code organiseren in functies"
aliases:
- /nl/powershell/organizing-code-into-functions.md
date:                  2024-01-28T22:03:00.012065-07:00
model:                 gpt-4-0125-preview
simple_title:         "Code organiseren in functies"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/powershell/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Code organiseren in functies gaat over het verpakken van codeblokken die specifieke taken uitvoeren en deze een naam geven. Dit wordt gedaan om code herbruikbaar, leesbaar en onderhoudbaar te maken. In plaats van dezelfde code opnieuw te schrijven, roep je een functie aan. Problemen oplossen of upgraden? Pas de functie aan zonder door stapels scripts te hoeven spitten.

## Hoe te:
Laten we een functie schrijven om de som van twee getallen te berekenen. Eenvoudig, maar het illustreert het punt.

```PowerShell
function Add-Numbers {
    param (
        [int]$FirstNum,
        [int]$SecondNum
    )
    return $FirstNum + $SecondNum
}

# Roep de functie aan met 5 en 10
$sum = Add-Numbers -FirstNum 5 -SecondNum 10
Write-Output "De som is $sum"
```

Voorbeelduitvoer:

```
De som is 15
```

## Diep Duiken
Functies in PowerShell, net als in de meeste talen, zijn oud nieuws. We hebben code al gecompartimentaliseerd sinds de dagen van Fortran. Het gaat over 'het wiel niet opnieuw uitvinden'. Alternatieven? Zeker, scripts of cmdlets. Maar deze missen de netheid en contextgevoeligheid van functies binnen scripts.

Implementatie? Functies kunnen basis zijn zoals ons voorbeeld of complex met scopes, pipeline-invoer en meer. Neem `Geavanceerde Functies`. Ze bootsen cmdlets na met parameters die attributen hebben, zoals `[Parameter(Mandatory=$true)]`. Dat is een voorproefje van de flexibiliteit van PowerShell.

## Zie Ook
- [about_Functions_Advanced_Parameters](https://docs.microsoft.com/nl-nl/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7.1)
- [about_Script_Blocks](https://docs.microsoft.com/nl-nl/powershell/module/microsoft.powershell.core/about/about_script_blocks?view=powershell-7.1)
