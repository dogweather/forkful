---
title:                "Tarkistetaan, onko hakemisto olemassa"
aliases:
- /fi/powershell/checking-if-a-directory-exists/
date:                  2024-02-03T19:08:15.949984-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
