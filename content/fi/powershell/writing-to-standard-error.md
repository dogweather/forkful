---
title:                "Tietokoneohjelmoinnin artikkelin otsikko: Kirjoittaminen standardivirheeseen"
html_title:           "PowerShell: Tietokoneohjelmoinnin artikkelin otsikko: Kirjoittaminen standardivirheeseen"
simple_title:         "Tietokoneohjelmoinnin artikkelin otsikko: Kirjoittaminen standardivirheeseen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Mitä & Miksi? 
Kirjoittaminen standardivirheeseen tarkoittaa virheviestien kirjoittamista ohjelmasta terminaaliin. Tämä on kätevä tapa ilmoittaa virheistä ja varoituksista sekä helpottaa ohjelmiston kehittämistä ja vianmääritystä. 

## Miten:
Käytännössä tämä tapahtuu koodaamalla ```Write-Error "Olen virheviesti"``` ja tuloksena näkyy sitten `Olen virheviesti` terminaalissa.

## Syventävä sukellus:
Standardivirheen kirjoittaminen on ollut käytössä jo pitkään, ja se on yleinen tapa ohjelmoida PowerShellissä. Jos haluat vaihtoehtoja, `Write-Warning` ja `Throw` komennot toimivat myös ilmoittaessa virheistä.

## Liittyvää:
Lisätietoja PowerShellin Standardivirheen kirjoittamisesta löydät täältä: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_redirection?view=powershell-7