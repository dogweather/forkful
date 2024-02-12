---
title:                "Testien kirjoittaminen"
date:                  2024-02-03T19:31:42.797877-07:00
model:                 gpt-4-0125-preview
simple_title:         "Testien kirjoittaminen"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Testien kirjoittaminen PowerShellillä tarkoittaa skriptien luomista, jotka automaattisesti varmistavat PowerShell-koodisi toiminnallisuuden, taaten sen toimivan odotetulla tavalla. Ohjelmoijat tekevät tämän havaitakseen virheet ajoissa, yksinkertaistaakseen koodin ylläpitoa ja varmistaakseen, että koodimuutokset eivät tahattomasti riko olemassa olevaa toiminnallisuutta.

## Miten:

PowerShell ei sisällä sisäänrakennettua testauskehystä, mutta Pester, suosittu kolmannen osapuolen moduuli, on laajalti käytetty testien kirjoittamiseen ja suorittamiseen. Näin pääset alkuun Pesterin kanssa testataksesi PowerShell-funktioitasi.

Ensin, asenna Pester, jos et ole vielä tehnyt niin:

```powershell
Install-Module -Name Pester -Scope CurrentUser -Force
```

Seuraavaksi, oletetaan että sinulla on yksinkertainen PowerShell-funktio, jonka haluat testata, tallennettuna nimellä `MyFunction.ps1`:

```powershell
function Get-MultipliedNumber {
    param (
        [int]$Number,
        [int]$Multiplier = 2
    )

    return $Number * $Multiplier
}
```

Testataksesi tätä funktiota Pesterin avulla, luo testiskripti nimeltä `MyFunction.Tests.ps1`. Tässä skriptissä, käytä Pesterin `Describe` ja `It` lohkoja määritelläksesi testitapaukset:

```powershell
# Tuo testattava funktio
. .\MyFunction.ps1

Describe "Get-MultipliedNumber tests" {
    It "Kertoo numeron kahdella, kun kertojaa ei ole annettu" {
        $result = Get-MultipliedNumber -Number 3
        $result | Should -Be 6
    }

    It "Kertoo numeron oikein annetulla kertojalla" {
        $result = Get-MultipliedNumber -Number 3 -Multiplier 3
        $result | Should -Be 9
    }
}
```

Testien suorittamiseksi, avaa PowerShell, siirry hakemistoon, jossa testiskriptisi sijaitsee, ja käytä `Invoke-Pester` komentoa:

```powershell
Invoke-Pester .\MyFunction.Tests.ps1
```

Esimerkkituloste näyttää tältä, ilmaisten onko testisi läpäisseet vai epäonnistuneet:

```
Starting discovery in 1 files.
Discovery finished in 152ms.
[+] C:\polku\to\MyFunction.Tests.ps1 204ms (182ms|16ms)
Tests completed in 204ms
Tests Passed: 2, Failed: 0, Skipped: 0 NotRun: 0
```

Tämä tuloste osoittaa, että molemmat testit ovat läpäisseet, antaen sinulle varmuuden siitä, että `Get-MultipliedNumber` funktiosi toimii odotetusti testaamissasi skenaarioissa.
