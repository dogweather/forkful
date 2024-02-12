---
title:                "Tekstitiedoston lukeminen"
aliases:
- /fi/powershell/reading-a-text-file/
date:                  2024-01-20T17:54:49.027823-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tekstitiedoston lukeminen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä ja Miksi?)
Tekstitiedoston lukeminen tarkoittaa tiedon hakemista tavallisen tekstimuotoisen tiedoston sisältä. Ohjelmoijat lukevat tekstitiedostoja, koska monet sovellukset tallentavat dataa näihin tiedostoihin, ja niitä on helppo muokata sekä lukea ilman erikoisohjelmia.

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
