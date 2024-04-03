---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:54.814583-07:00
description: "Tekstitiedoston kirjoittaminen PowerShelliss\xE4 k\xE4sitt\xE4\xE4 tekstipohjaisten\
  \ tiedostojen luomisen ja k\xE4sittelyn, mik\xE4 on perustoiminto lokitiedostojen,\
  \ datan\u2026"
lastmod: '2024-03-13T22:44:56.800854-06:00'
model: gpt-4-0125-preview
summary: "Tekstitiedoston kirjoittaminen PowerShelliss\xE4 k\xE4sitt\xE4\xE4 tekstipohjaisten\
  \ tiedostojen luomisen ja k\xE4sittelyn, mik\xE4 on perustoiminto lokitiedostojen,\
  \ datan tallennuksen ja konfigurointiskriptien kannalta."
title: Tekstitiedoston kirjoittaminen
weight: 24
---

## Mikä ja miksi?
Tekstitiedoston kirjoittaminen PowerShellissä käsittää tekstipohjaisten tiedostojen luomisen ja käsittelyn, mikä on perustoiminto lokitiedostojen, datan tallennuksen ja konfigurointiskriptien kannalta. Ohjelmoijat hyödyntävät tätä järjestelmätehtävien automatisoimiseen, datan analysointiin ja integroimiseen muiden sovellusten tai skriptien kanssa.

## Kuinka:
PowerShell tarjoaa suoraviivaisia cmdlet-komentoja tiedostojen käsittelyyn. `Out-File` cmdlet ja uudelleenohjausoperaattorit ovat ensisijaisesti käytössä tähän tarkoitukseen. Tässä on esimerkkejä, jotka havainnollistavat tekstin kirjoittamista tiedostoihin eri skenaarioissa:

**Perustekstitiedoston luominen:**

Luodaksesi tekstitiedoston ja kirjoittaaksesi siihen yksinkertaisen merkkijonon, voit käyttää:

```powershell
"Hello, World!" | Out-File -FilePath .\example.txt
```

Tai vastaavasti uudelleenohjausoperaattorilla:

```powershell
"Hello, World!" > .\example.txt
```

**Tekstin lisääminen olemassa olevaan tiedostoon:**

Jos haluat lisätä tekstiä olemassa olevan tiedoston loppuun kirjoittamatta sen yli:

```powershell
"Another line." | Out-File -FilePath .\example.txt -Append
```

Tai käyttäen lisäyksen uudelleenohjausoperaattoria:

```powershell
"Another line." >> .\example.txt
```

**Usean rivin kirjoittaminen:**

Usean rivin kirjoittamiseen voit käyttää merkkijonojen taulukkoa:

```powershell
$lines = "Line 1", "Line 2", "Line 3"
$lines | Out-File -FilePath .\multilines.txt
```

**Merkistökoodauksen määrittäminen:**

Tietyn tekstikoodauksen määrittämiseksi käytä `-Encoding` parametria:

```powershell
"Text with UTF8 Encoding" | Out-File -FilePath .\utfexample.txt -Encoding UTF8
```

**Kolmannen osapuolen kirjastojen käyttö:**

Vaikka PowerShelliin sisäänrakennetut cmdlet-komennot riittävätkin perustason tiedosto-operaatioihin, monimutkaisemmat tehtävät saattavat hyötyä kolmannen osapuolen moduuleista, kuten `PowershellGet`, tai Windowsiin portatuista työkaluista, kuten `SED` ja `AWK`. Kuitenkin, pelkästään tekstitiedoston kirjoittamiseen, nämä saattavat olla liioittelua eivätkä yleensä ole tarpeen:

```powershell
# Olettaen, että monimutkaisempi skenaario perusteli ulkoisen kirjaston käytön
# Install-Module -Name SomeComplexLibrary
# Import-Module -Name SomeComplexLibrary
# Monimutkaisempia operaatioita tässä
```

_Huom: Harkitse aina, onko kolmannen osapuolen riippuvuuden lisäämisen monimutkaisuus oikeutettu tarpeisiisi nähden._

**Esimerkkitulos:**

Suorittaessasi perustason tiedostonluontikomentoa, `example.txt` tiedoston sisällön tarkistaminen näyttää:

```plaintext
Hello, World!
```

Tekstin lisäämisen ja `example.txt` tiedoston tarkistamisen jälkeen:

```plaintext
Hello, World!
Another line.
```
