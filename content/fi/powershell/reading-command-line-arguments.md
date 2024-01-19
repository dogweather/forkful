---
title:                "Komentorivin argumenttien lukeminen"
html_title:           "Bash: Komentorivin argumenttien lukeminen"
simple_title:         "Komentorivin argumenttien lukeminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Komennonriviargumenttien lukeminen on prosessi, jossa ohjelma saa syötteenä arvoja komentoriviltä aloittaessaan. Ohjelmoijat tekevät tämän monista syistä, kuten määrittääkseen ohjelman käyttäytymisen tai välttääkseen koodin liiallisen joustavuuden.

## Miten:

```PowerShell
# Tämä on yksinkertainen esimerkki PowerShell-skriptistä, joka lukee komennonriviargumentteja.

Param(
   [Parameter(Mandatory=$true)][string]$argumentti1,
   [Parameter(Mandatory=$false)][string]$argumentti2
)

Write-Host "Argumentti 1: " $argumentti1
Write-Host "Argumentti 2: " $argumentti2
```

Tulosteessa nähdään:

```PowerShell
> .\Skripti.ps1 -argumentti1 "Moi" -argumentti2 "Maailma"
Argumentti 1: Moi
Argumentti 2: Maailma
```

## Syvällisempi tarkastelu

Komennonriviargumenttien lukeminen juontaa juurensa varhaisiin tietokoneisiin, jolloin ne olivat ainoa tapa syöttää ohjelmiin tietoja käytettävän graafisen käyttöliittymän puuttuessa. Nämä menetelmät ovat säilyneet, koska ne ovat yksinkertaisia ja tehokkaita.

PowerShellissa on useita tapoja lukea komennonriviargumentteja. Kuten esimerkkikoodissa käytetty Param, Get-Args on toinen tapa käsitellä argumentteja, joiden avulla pääset käsiksi jokaiseen yksittäiseen lähettämääsi argumenttiin.

Totuus on, että komennonriviargumenttien lukeminen on haluttu taito ohjelmoijille, koska se antaa joustavuuden määrittää ohjelman suorituksen aikana tarvittavat asiat.

## Katso myös

1. Microsoft PowerShell Docs - Param: https://docs.microsoft.com/fi-fi/powershell/scripting/learn/deep-dives/everything-about-parameter-validation?view=powershell-7.1
2. MSDN - Komentoriviargumentit: https://docs.microsoft.com/fi-fi/windows-server/administration/windows-commands/windows-commands
3. PowerShell.org - Käsittelee komennonriviargumentteja: https://powershell.org/2013/11/24/powershell-args-and-what-to-do-with-them/

Muista, että näiden linkkien informaatio on tärkeää ja voi auttaa ymmärtämään paremmin komennonriviargumenttien käyttöä.