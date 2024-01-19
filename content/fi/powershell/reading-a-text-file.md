---
title:                "Tekstitiedoston lukeminen"
html_title:           "Lua: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?
Tekstitiedoston lukeminen tarkoittaa tiedostosta tiedon, esimerkiksi asetusten tai käyttäjän syötteiden, hakemista. Ohjelmoijat lukevat näitä tiedostoja, koska sieltä saadaan arvokasta tietoa, jota voidaan käyttää ohjelman tai skriptin logiikassa.

# Kuinka:
PowerShellin avulla tekstitiedostoja voidaan lukea seuraavasti:

```PowerShell
$teksti = Get-Content -Path "C:\polku\tiedosto.txt"
$x = 0
foreach($rivi in $teksti){
    $x++
    Write-Output "Rivi $x : $rivi"
}
```
Yllä oleva koodi lukee tekstitiedoston rivit ja tulostaa ne numerojärjestyksessä konsoliin.

# Syvällinen tieto:
Tiedostojen lukeminen PowerShellissa on suoraviivaista, mutta sen takana on pitkä historia. Tämä toiminnallisuus periytyy DOS-järjestelmän `TYPE`-komennosta. PowerShellissa on muitakin vaihtoehtoja tiedoston lukemiseen, esimerkiksi `Import-Csv`- ja `ConvertFrom-Json`-komennot, jotka on suunniteltu erityisesti tietyntyyppisten tiedostojen lukemiseen.

Tiedostojen lukemisen toteutus PowerShellissa on äärimmäisen tehokas. `Get-Content`-komento kykenee käsittelemään suuriakin tiedostoja, koska se lukee tiedoston rivi kerrallaan muistin säästämiseksi.

# Katso lisää:
- Get-Content-komennon virallinen dokumentaatio: [https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content)
- PowerShellin perusteet: [https://docs.microsoft.com/en-us/powershell/scripting/overview](https://docs.microsoft.com/en-us/powershell/scripting/overview)
- PowerShell-oppaat: [https://www.tutorialspoint.com/powershell/index.htm](https://www.tutorialspoint.com/powershell/index.htm)