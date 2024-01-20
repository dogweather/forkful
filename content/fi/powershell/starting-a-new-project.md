---
title:                "Aloittaminen uuden projektin"
html_title:           "C: Aloittaminen uuden projektin"
simple_title:         "Aloittaminen uuden projektin"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Uuden Projektin Aloitus PowerShellilla

## Mikä ja Miksi?
Uuden projektin aloitus viittaa tyhjästä aloittamiseen uudella ohjelmointitehtävällä. Ohjelmoijat tekevät tämän joko ryhtyessään uusiin itsenäisiin projekteihin tai aloittaessaan uudet tehtävänsä palvelussa tai yrityksessä.

## Miten se tehdään:
PowerShellissa uuden projektin, esimerkiksi skriptiprojektin, voi aloittaa muutamalla peruskomennolla. Tässä on hyvä esimerkki:

```PowerShell
# Luo uusi kansio projektille
New-Item -Path 'c:\MinunProjektini' -ItemType Directory

# Luo uusi PowerShell-skriptitiedosto
New-Item -Path 'c:\MinunProjektini\MinunSkriptini.ps1' -ItemType File
```

Nyt, kun suoritat edellä mainitut komentosarjat, saat seuraavanlaisen tulosteen:

```PowerShell
Directory: C:\

Mode                 LastWriteTime         Length Name
----                 -------------         ------ ----
d-----        10.07.2022     12:02                MinunProjektini


Directory: C:\MinunProjektini

Mode                 LastWriteTime         Length Name
----                 -------------         ------ ----
-a----        10.07.2022     12:02              0 MinunSkriptini.ps1
```

## Syväsukellus
Uuden projektin aloitus PowerShellilla ei ole uusi käsite. Alun perin syntyneenä vuonna 2006 osana Windows Vista -käyttöjärjestelmää, PowerShell on tullut pitkän matkan, ja sen käyttäminen projektin aloittamiseen on tullut entistä yksinkertaisemmaksi.

Vaihtoehtoisia tapoja uuden projektin aloittamiseen ovat käyttöjärjestelmäkohtaiset komennot tai graafiset käyttöliittymät, kuten Microsoft Visual Studio. Huomaa, että käytettävissä olevat vaihtoehdot voivat riippua käyttämästäsi ohjelmointikielestä ja/tai kehitysympäristöstä.

PowerShellissa uuden projektin aloitus keskittyy yleensä kansiorakenteen luomiseen ja tarvittavien tiedostojen määrittämiseen. Tämä kannattaa pitää mielessä, kun aloitat oman projektisi.

## Katso myös
Microsoftin viralliset PowerShell-dokumentit: https://docs.microsoft.com/en-us/powershell/
Microsoft Learn PowerShell-kurssi: https://docs.microsoft.com/en-us/learn/paths/powershell/

Hyvä alusta aloittaa uusia projekteja PowerShellilla!