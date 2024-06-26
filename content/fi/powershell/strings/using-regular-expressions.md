---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:56.443407-07:00
description: "Miten: PowerShellissa voit k\xE4ytt\xE4\xE4 `-match`, `-replace` ja\
  \ `-split` operaattoreita, muun muassa, suorittamaan toimintoja s\xE4\xE4nn\xF6\
  llisten lausekkeiden\u2026"
lastmod: '2024-03-13T22:44:56.770436-06:00'
model: gpt-4-0125-preview
summary: "PowerShellissa voit k\xE4ytt\xE4\xE4 `-match`, `-replace` ja `-split` operaattoreita,\
  \ muun muassa, suorittamaan toimintoja s\xE4\xE4nn\xF6llisten lausekkeiden avulla."
title: "S\xE4\xE4nn\xF6llisten lausekkeiden k\xE4ytt\xF6"
weight: 11
---

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
