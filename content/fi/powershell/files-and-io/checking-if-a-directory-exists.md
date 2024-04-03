---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:15.949984-07:00
description: "Miten: PowerShell tarjoaa suoraviivaisen tavan tarkistaa hakemiston\
  \ l\xE4sn\xE4olo k\xE4ytt\xE4m\xE4ll\xE4 `Test-Path`-cmdletia. T\xE4m\xE4 cmdlet\
  \ palauttaa Boolean-arvon, joka\u2026"
lastmod: '2024-03-13T22:44:56.797003-06:00'
model: gpt-4-0125-preview
summary: "PowerShell tarjoaa suoraviivaisen tavan tarkistaa hakemiston l\xE4sn\xE4\
  olo k\xE4ytt\xE4m\xE4ll\xE4 `Test-Path`-cmdletia."
title: Tarkistetaan, onko hakemisto olemassa
weight: 20
---

## Miten:
PowerShell tarjoaa suoraviivaisen tavan tarkistaa hakemiston läsnäolo käyttämällä `Test-Path`-cmdletia. Tämä cmdlet palauttaa Boolean-arvon, joka ilmaisee, onko määritetty polku olemassa. Näin voit käyttää sitä:

```powershell
# Tarkista, onko hakemisto olemassa
$directoryPath = "C:\ExamplePath"
$directoryExists = Test-Path -Path $directoryPath
Write-Output "Onko hakemisto olemassa? $directoryExists"
```

Esimerkkituloste olemassa olevalle hakemistolle:

```
Onko hakemisto olemassa? True
```

Ja hakemistolle, jota ei ole olemassa:

```
Onko hakemisto olemassa? False
```

Monimutkaisempia skriptejä varten, erityisesti niitä, jotka vuorovaikuttavat verkon jaossa tai pilvitallennustilassa, saattaa tarvita lisätarkistuksia tai toiminnallisuuksia, joita ei suoraan ole saatavilla `Test-Path`-kautta. Tällaisissa tapauksissa kolmansien osapuolten PowerShell-moduulien tai kirjastojen hyödyntäminen voi olla hyödyllistä, vaikkakin useimmat rutiinitehtävät voidaan suorittaa PowerShellin sisäänrakennetuilla cmdleteillä. Viimeisimmän tietoni mukaan, laajalti hyväksyttyä kolmannen osapuolen kirjastoa tarkistamaan hakemiston olemassaoloa sen yli mitä `Test-Path` tarjoaa, ei ole ollut, pääasiassa koska `Test-Path` itsessään on sekä robusti että tehokas tähän tarkoitukseen.
