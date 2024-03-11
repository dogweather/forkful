---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:56.443407-07:00
description: "S\xE4\xE4nn\xF6lliset lausekkeet (regex) ovat merkkijonoja, jotka muodostavat\
  \ hakukaavan, ja niit\xE4 k\xE4ytet\xE4\xE4n p\xE4\xE4asiassa merkkijonojen haussa\
  \ ja k\xE4sittelyss\xE4.\u2026"
lastmod: '2024-03-11T00:14:30.728031-06:00'
model: gpt-4-0125-preview
summary: "S\xE4\xE4nn\xF6lliset lausekkeet (regex) ovat merkkijonoja, jotka muodostavat\
  \ hakukaavan, ja niit\xE4 k\xE4ytet\xE4\xE4n p\xE4\xE4asiassa merkkijonojen haussa\
  \ ja k\xE4sittelyss\xE4.\u2026"
title: "S\xE4\xE4nn\xF6llisten lausekkeiden k\xE4ytt\xF6"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Säännölliset lausekkeet (regex) ovat merkkijonoja, jotka muodostavat hakukaavan, ja niitä käytetään pääasiassa merkkijonojen haussa ja käsittelyssä. Ohjelmoijat hyödyntävät regexiä PowerShellissä tehtäviin, kuten datan validointiin, jäsentämiseen ja muunteluun, sen tehokkuuden ja joustavuuden ansiosta monimutkaisten kaavojen käsittelyssä.

## Miten:

PowerShellissa voit käyttää `-match`, `-replace` ja `-split` operaattoreita, muun muassa, suorittamaan toimintoja säännöllisten lausekkeiden avulla. Tutkitaan muutamia esimerkkejä:

### Käyttäen `-match`-operaattoria tarkistamaan, vastaako merkkijono kaavaa
Tämä operaattori palauttaa `$true`, jos kaava löytyy merkkijonosta, ja `$false` muussa tapauksessa.

```powershell
"hello world" -match "\w+orld"
# Tuloste: True
```

### Otteluiden poimiminen
Voit poimia vastaavan arvon käyttämällä automaattista muuttujaa `$matches`.

```powershell
if ("I have 100 apples" -match "\d+") {
    "Numero löydetty: " + $matches[0]
}
# Tuloste: Numero löydetty: 100
```

### Käyttäen `-replace`-operaattoria korvauksiin
`-replace` operaattori korvaa kaikki kaavan esiintymät määritellyllä korvaavalla merkkijonolla.

```powershell
"foo bar baz" -replace "ba[rz]", "qux"
# Tuloste: foo qux qux
```

### Merkkijonojen jakaminen `-split`-operaattorin avulla
Jaa merkkijono taulukoksi alimerkkijonoista perustuen regex-kaavaan.

```powershell
"The quick-brown_fox jumps" -split "[-_ ]"
# Tuloste: The quick brown fox jumps
```

### Edistyneempi Kaavojen Vastaavuus
PowerShell tukee myös monimutkaisempia regex-toimintoja `[regex]` luokan kautta, antaen pääsyn metodeihin kuten `Matches()`, `Replace()`, ja `Split()`.

```powershell
[regex]::Matches("June 24, August 9, Dec 12", "\b[A-Za-z]+\b").Value
# Tuloste: June August Dec

[regex]::Replace("100,000", "\B(?=(?:\d{3})+(?!\d))", ",")
# Tuloste: 100,000

[regex]::Split("one,two;three four", ",|;| ")
# Tuloste: one two three four
```

Nämä esimerkit näyttävät säännöllisten lausekkeiden tehon ja monipuolisuuden PowerShellissä datan käsittelyssä ja kaavojen vastaavuudessa. Hyödyntämällä regexiä, ohjelmoijat voivat suorittaa monimutkaista tekstinkäsittelyä tehokkaasti.
