---
title:                "Väliaikaisen tiedoston luominen"
html_title:           "PowerShell: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mikä ja miksi? 

Väliaikaistiedostoa luotaessa luodaan tilapäinen tiedosto, jota käytetään vain hetkellisesti ohjelmoinnissa. Tämä voi esimerkiksi tapahtua silloin, kun ohjelma tarvitsee tallentaa väliaikaisia tietoja tai luoda uuden tyhjän tiedoston, jota se käyttää jatkotoimenpiteissä. Ohjelmoijat käyttävät väliaikaistiedostoja helpottaakseen työtään ja tehdäkseen koodistaan joustavampaa.

## Kuinka tehdä:

```PowerShell
# Luo uusi väliaikaistiedosto
New-TemporaryFile 

# Ilmoita uudelle tiedostolle haluttu nimi ja paikka käyttäen muuttujia
$tempFile = New-TemporaryFile -Path C:\Users\$env:USERNAME\Desktop -Name "tempfile.txt"

# Luo ja tallenna data väliaikaistiedostoon
$data = "Tämä on esimerkkidataa"
Set-Content -Path $tempFile.FullName -Value $data

# Näytä tiedoston sisältö
Get-Content -Path $tempFile.FullName 

```

Tämä esimerkki luo uuden väliaikaistiedoston, antaa sille nimeksi "tempfile.txt" ja sijoittaa sen käyttäjän työpöydälle. Sitten se tallentaa tekstin "Tämä on esimerkkidataa" tiedostoon ja näyttää sen sisällön komennolla "Get-Content".

## Syvemmällä:

Aikaisemmin väliaikaistiedostojen luominen vaati usein manuaalista koodausta ja tiedostojenhallinnan käsittelyä. PowerShellin avulla tämä prosessi on nyt huomattavasti helpompaa ja joustavampaa. Väliaikaistiedostojen käyttöä voidaan myös välttää, jos niitä ei tarvita, ja sen sijaan ohjelma voi tallentaa tietoja suoraan pääasialliseen tiedostoon.

## Katso myös:

[PowerShell Documentation: New-TemporaryFile](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/new-temporaryfile?view=powershell-7.1)

[PowerShell Scripting Tips: Creating and Deleting Temporary Files](https://powertips.org/creating-deleting-temp-files/)