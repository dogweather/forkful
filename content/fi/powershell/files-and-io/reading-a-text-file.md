---
date: 2024-01-20 17:54:49.027823-07:00
description: "How to: (Kuinka tehd\xE4\xE4n:) PowerShellissa tekstitiedoston lukeminen\
  \ on suoraviivaista. T\xE4ss\xE4 pari esimerkki\xE4: Tiedoston lukeminen kokonaisuudessaan."
lastmod: '2024-04-05T21:53:58.375640-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4\xE4n:) PowerShellissa tekstitiedoston lukeminen on suoraviivaista."
title: Tekstitiedoston lukeminen
weight: 22
---

## How to: (Kuinka tehdään:)
PowerShellissa tekstitiedoston lukeminen on suoraviivaista. Tässä pari esimerkkiä:

Tiedoston lukeminen kokonaisuudessaan:
```PowerShell
$content = Get-Content -Path 'example.txt'
Write-Output $content
```

Rivi riviltä lukeminen:
```PowerShell
Get-Content -Path 'example.txt' | ForEach-Object {
    Write-Output $_
}
```

Tuloste näyttää tiedoston sisällön konsolissa.

## Deep Dive (Syväsukellus)
History: PowerShellin edeltäjä oli Command Prompt ja sen batch-skriptaus, mutta PowerShell toi mukanaan tehokkaamman ja objekti-pohjaisen lähestymistavan automatisointiin ja hallintaan Windowsissa.

Vaihtoehdot: PowerShellin lisäksi tiedostoja voi lukea erilaisilla ohjelmointikielillä kuten Pythonilla, Javalla, tai vaikkapa Bash-skriptillä Linuxissa. Jokaisella on omat hyvät ja huonot puolensa.

Toteutuksen yksityiskohdat: `Get-Content` cmdlet lukee tiedoston sisällön ja palauttaa sen joko merkkijonona tai merkkijonojen taulukkona. ISOissa tiedostoissa on suositeltavaa lukea tiedoston sisältö riveittäin muistinkäytön tehostamiseksi.

## See Also (Katso myös)
- [PowerShellin skriptausohjeet Microsoftin sivuilla](https://docs.microsoft.com/en-us/powershell/scripting/overview)
- [Get-Content cmdletin käyttöohje](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content)
- [about_Automatic_Variables, PowerShell-dokumentaatio (esim. `$_` käytöstä)](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables)
