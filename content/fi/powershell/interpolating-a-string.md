---
title:                "Merkkijonon interpolointi"
aliases:
- fi/powershell/interpolating-a-string.md
date:                  2024-01-20T17:51:43.245769-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon interpolointi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä ja Miksi?)
Interpoloinnissa yhdistetään muuttujia ja kiinteää tekstiä yhdeksi merkkijonoksi. Koodarit käyttävät tätä menetelmää siksi, että se tekee koodista selkeämpää ja dynaamista sisällön yhdistämisen kautta.

## How to: (Kuinka tehdä:)
```PowerShell
# Perusesimerkki
$name = 'Heikki'
$age = 32
$greeting = "Hei, nimeni on $name ja olen $age vuotta vanha."
Write-Output $greeting
```
Tuloste: Hei, nimeni on Heikki ja olen 32 vuotta vanha.

```PowerShell
# Ilmaisuja käyttäen
$items = 10
$totalCost = 19.99
$receipt = "Sinun {0} tuotetta maksaa yhteensä {1:C}." -f $items, $totalCost
Write-Output $receipt
```
Tuloste: Sinun 10 tuotetta maksaa yhteensä 19,99 €.

```PowerShell
# Here-stringin sisällä
$scriptBlock = {
    param($userName)
    @"
Käyttäjän '$userName' prosessit:
$(Get-Process -user $userName)
"@
}
$report = & $scriptBlock -userName 'Mikko'
Write-Output $report
```

## Deep Dive (Syväsukellus)
Stringien interpolointi otettiin käyttöön PowerShellissä version 2.0 aikaan ja sitä on sen jälkeen käytetty laajalti. Vaihtoehtona on käyttää -f operaattoria tai yhdistellä stringejä plussa (+) merkillä, mutta nämä voivat olla sekavampia ja vähemmän tehokkaita. Interpoloitavat alueet merkitään $-merkillä tai laajennetussa merkkijonossa käyttäen "@-merkkijonoja". PowerShell laskee ja korvaa muuttujat ja lausekkeet automaattisesti merkkijonon sisällä.

## See Also (Katso Myös)
- [About Quoting Rules](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_quoting_rules)
- [About Special Characters](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_special_characters)
- [Format Operator -f](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_operators#format-operator--f)
