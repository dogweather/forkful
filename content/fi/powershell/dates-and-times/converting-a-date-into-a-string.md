---
date: 2024-01-20 17:37:26.096736-07:00
description: "Muunnetaan p\xE4iv\xE4m\xE4\xE4r\xE4 merkkijonoksi. Se puree tiedot\
  \ ihmisen luettavaan muotoon tai valmistelee ne tallennusta varten. Tiedon esitt\xE4\
  minen tietyss\xE4\u2026"
lastmod: '2024-03-13T22:44:56.793965-06:00'
model: gpt-4-1106-preview
summary: "Muunnetaan p\xE4iv\xE4m\xE4\xE4r\xE4 merkkijonoksi."
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n muuntaminen merkkijonoksi"
weight: 28
---

## How to: (Kuinka Tehdä:)
PowerShellissa päivämäärää merkkijonoksi muuttaminen käyttää `Get-Date` cmdletiä ja sen `-Format` parametria.

```PowerShell
# Perusmuoto
$pvm = Get-Date
$pvm.ToString()

# Formaatti mukautetulla tavalla
$pvm_string = $pvm.ToString("yyyy-MM-dd")
Write-Output $pvm_string

# Tai käytä -Format parametria
$formatted = Get-Date -Format "dd.MM.yyyy HH:mm"
Write-Output $formatted
```

Tulosteesimerkit:
```
04.04.2023 19:45:17
2023-04-04
04.04.2023 19:45
```

## Deep Dive (Syväsukellus):
Päivämäärän muuntaminen merkkijonoksi on ollut osa skriptikieliä iät ja ajat, koska päivämäärät ovat olleet tietokoneohjelmoinnin alkuajoista lähtien tärkeitä. PowerShell versiosta 1.0 lähtien, `Get-Date` cmdlet on antanut helpon tavan käsitellä päivämääriä.

Via laajennetun `ToString()` metodin tai `-Format` parametrin, voit määrittää päivämääräformaatteja käyttäen .NET formaatin merkkijonoja. Erilaiset kulttuuriasetukset (esim. `fi-FI`) vaikuttavat oletusformaattiin.

Kiinnitä huomiota aluekohtaisten asetusten ja UTC-aikojen hallintaan. PowerShellissa voi kääntää päivämäärän UTC:ksi käyttäen `ToUniversalTime()` metodia.

## See Also (Katso Myös):
- Microsoftin dokumentaatio `Get-Date`: https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/get-date
- .NET Päivämäärä ja Aika formaatit: https://docs.microsoft.com/dotnet/standard/base-types/standard-date-and-time-format-strings
- PowerShell aikavyöhykkeiden hallinta: https://docs.microsoft.com/powershell/module/microsoft.powershell.management/get-timezone
