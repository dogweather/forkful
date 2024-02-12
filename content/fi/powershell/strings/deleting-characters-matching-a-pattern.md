---
title:                "Merkkien poistaminen hakemalla osumia kaavaan"
aliases:
- /fi/powershell/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:43:04.314656-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkien poistaminen hakemalla osumia kaavaan"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Pattern matching on poistaa hahmoja merkkijonosta, jotka vastaavat tiettyä kaavaa. Ohjelmoijat tekevät sen, koska se auttaa puhdistamaan ja muokkaamaan dataa - esimerkiksi käyttäjän syötteestä tai tiedostoista.

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
