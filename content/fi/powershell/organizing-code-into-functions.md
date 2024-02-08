---
title:                "Koodin järjestäminen funktioihin"
aliases:
- fi/powershell/organizing-code-into-functions.md
date:                  2024-01-26T01:11:48.995983-07:00
model:                 gpt-4-1106-preview
simple_title:         "Koodin järjestäminen funktioihin"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Koodin järjestäminen funktioihin tarkoittaa koodirykelmien käärimistä, jotka suorittavat tiettyjä tehtäviä ja niille annetaan nimi. Tämä tehdään koodin uudelleenkäytettävyyden, luettavuuden ja ylläpidettävyyden vuoksi. Sen sijaan, että kirjoittaisi saman koodin uudelleen, kutsu funktiota. Haluatko vianetsiä tai päivittää? Säädä funktiota ilman, että sinun tarvitsee penkoa läjää skriptejä.

## Kuinka:
Kirjoitetaan funktio kahden luvun summan laskemiseen. Yksinkertainen, mutta se havainnollistaa pointtia.

```PowerShell
function Add-Numbers {
    param (
        [int]$FirstNum,
        [int]$SecondNum
    )
    return $FirstNum + $SecondNum
}

# Kutsu funktiota arvoilla 5 ja 10
$sum = Add-Numbers -FirstNum 5 -SecondNum 10
Write-Output "Summa on $sum"
```

Esimerkkituloste:

```
Summa on 15
```

## Syväluotaus
Funktiot PowerShellissä, kuten useimmissa kielissä, ovat vanha juttu. Olemme jakaneet koodia osiin jo Fortranin päivien aikana. Kyse on 'pyörän uudelleenkeksimisen' välttämisestä. Vaihtoehtoja? Toki, skriptejä tai cmdletejä. Mutta ne eivät tarjoa funktioiden siistiä ja kontekstille herkkää olemusta skriptien sisällä.

Toteutus? Funktiot voivat olla perusmallisia, kuten esimerkkimme, tai monimutkaisia, joissa on soveltamisalueita, putkilinjasisääntuloa ja enemmän. Ota `Edistyneet Funktiot`. Ne jäljittelevät cmdletejä parametreillä, joilla on attribuutteja, kuten `[Parameter(Mandatory=$true)]`. Se on esimakua PowerShellin joustavuudesta.

## Katso myös
- [about_Functions_Advanced_Parameters](https://docs.microsoft.com/fi-fi/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7.1)
- [about_Script_Blocks](https://docs.microsoft.com/fi-fi/powershell/module/microsoft.powershell.core/about/about_script_blocks?view=powershell-7.1)
