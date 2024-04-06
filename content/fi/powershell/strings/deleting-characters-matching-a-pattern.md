---
date: 2024-01-20 17:43:04.314656-07:00
description: "How to: (Kuinka tehd\xE4:) PowerShell k\xE4ytt\xE4\xE4 .NETin regex-moottoria\
  \ mallien tunnistamiseen, joka on per\xE4isin 2000-luvun alusta. Vaihtoehtoja kuvioille\u2026"
lastmod: '2024-04-05T22:51:10.917624-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) PowerShell k\xE4ytt\xE4\xE4 .NETin regex-moottoria mallien\
  \ tunnistamiseen, joka on per\xE4isin 2000-luvun alusta."
title: Merkkien poistaminen hakemalla osumia kaavaan
weight: 5
---

## How to: (Kuinka tehdä:)
```PowerShell
# Poista numeroita merkkijonosta
$string = "PS5 on konsolipelien huippu vuonna 2023!"
$cleanString = $string -replace '[0-9]', ''
Write-Output $cleanString
# Tulostaa: PS on konsolipelien huippu vuonna !

# Poista erikoismerkkejä lukuun ottamatta välilyöntejä
$string = "PowerShell > Batch scripts, am I right? ;)"
$cleanString = $string -replace '[^\w\s]', ''
Write-Output $cleanString
# Tulostaa: PowerShell  Batch scripts am I right 
```

## Deep Dive (Syväsukellus):
PowerShell käyttää .NETin regex-moottoria mallien tunnistamiseen, joka on peräisin 2000-luvun alusta. Vaihtoehtoja kuvioille vastaavien merkkien poistamiseen ovat .Trim(), .Replace() ja mukautettujen funktioiden kirjoittaminen. Regex on tehokas, se tukee monimutkaisia malleja ja ehdonalaisia operaatioita.

Käytettäessä '-replace' operaattoria, ensimmäinen parametri on malli, joka poistetaan, ja toinen on se, millä korvataan (tässä tapauksessa tyhjällä). Usein käyttäjä jättää toisen parametrin tyhjäksi poistaakseen merkit suoraan. Regexin syvä ymmärrys antaa voimakkaat työkalut datan käsittelyyn.

## See Also (Katso myös):
- [about_Regular_Expressions](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.1)
- [about_Comparison_Operators](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7.1)
- [PowerShell 7.1 Documentation](https://docs.microsoft.com/en-us/powershell/)
- [Regular-Expressions.info](https://www.regular-expressions.info/)
