---
aliases:
- /fi/powershell/checking-if-a-directory-exists/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:15.949984-07:00
description: "PowerShellissa hakemiston olemassaolon tarkistaminen on yleinen teht\xE4\
  v\xE4, joka auttaa skriptej\xE4 tekem\xE4\xE4n p\xE4\xE4t\xF6ksi\xE4 tiedostoj\xE4\
  rjestelm\xE4n rakenteeseen\u2026"
lastmod: 2024-02-18 23:09:07.869550
model: gpt-4-0125-preview
summary: "PowerShellissa hakemiston olemassaolon tarkistaminen on yleinen teht\xE4\
  v\xE4, joka auttaa skriptej\xE4 tekem\xE4\xE4n p\xE4\xE4t\xF6ksi\xE4 tiedostoj\xE4\
  rjestelm\xE4n rakenteeseen\u2026"
title: Tarkistetaan, onko hakemisto olemassa
---

{{< edit_this_page >}}

## Mikä & Miksi?
PowerShellissa hakemiston olemassaolon tarkistaminen on yleinen tehtävä, joka auttaa skriptejä tekemään päätöksiä tiedostojärjestelmän rakenteeseen perustuen—kuten välttämään virheitä vahvistamalla, että kohdehakemisto on paikallaan ennen luku- tai kirjoitusyritystä. Se on olennaista varmistaaksesi, että skriptisi toimii luotettavasti monenlaisissa ympäristöissä.

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
