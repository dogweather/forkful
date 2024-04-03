---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:34.842112-07:00
description: "Miten: Vaikka Fish Shelliss\xE4 ei ole sis\xE4\xE4nrakennettua komentoa\
  \ regexille, se k\xE4ytt\xE4\xE4 tehokkaasti ulkoisia komentoja kuten `grep`, `sed`\
  \ ja `awk`, jotka\u2026"
lastmod: '2024-03-13T22:44:56.982162-06:00'
model: gpt-4-0125-preview
summary: "Vaikka Fish Shelliss\xE4 ei ole sis\xE4\xE4nrakennettua komentoa regexille,\
  \ se k\xE4ytt\xE4\xE4 tehokkaasti ulkoisia komentoja kuten `grep`, `sed` ja `awk`,\
  \ jotka tukevat regexi\xE4, sallien sinun sis\xE4llytt\xE4\xE4 regex-operaatioita\
  \ skripteihisi."
title: "S\xE4\xE4nn\xF6llisten lausekkeiden k\xE4ytt\xF6"
weight: 11
---

## Miten:
Vaikka Fish Shellissä ei ole sisäänrakennettua komentoa regexille, se käyttää tehokkaasti ulkoisia komentoja kuten `grep`, `sed` ja `awk`, jotka tukevat regexiä, sallien sinun sisällyttää regex-operaatioita skripteihisi.

### Peruskuvioiden Vastaavuus `grep`-käytössä
Etsi tiedostosta rivejä, jotka vastaavat mallia:

```fish
grep '^[0-9]+' myfile.txt
```

Tämä komento etsii `myfile.txt`-tiedostosta rivejä, jotka alkavat yhdellä tai useammalla numerolla.

### Uuttaminen & Korvaaminen `sed`-käytössä
Uuta puhelinnumerot tiedostosta:

```fish
sed -n '/\([0-9]\{3\}\)-\([0-9]\{3\}\)-\([0-9]\{4\}\)/p' contacts.txt
```

Korvaa kaikki "foo"-esiintymät "bar"-sanalla `data.txt`-tiedostossa:

```fish
sed 's/foo/bar/g' data.txt
```

### Käyttäen `string`-komentoa Perus Regexille
Vastaa mallia merkkijonossa:

```fish
echo "fish 3.1.2" | string match -r '3\.[0-9]+\.[0-9]+'
```
Tuloste:
```
3.1.2
```

Korvaa numerot 'fish'-sanan jälkeen 'X.X.X':lla:

```fish
echo "Welcome to fish 3.1.2" | string replace -ra '([fish]+\s)[0-9\.]+' '$1X.X.X'
```
Tuloste:
```
Welcome to fish X.X.X
```

### Kehittynyt Vastaavuus `awk`-käytössä
Tulosta toinen sarake tiedoista, missä ensimmäinen sarake vastaa tiettyä mallia:

```fish
awk '$1 ~ /^a[0-9]+$/ {print $2}' datafile
```

Tämä komento etsii `datafile`-tiedostosta rivejä, joissa ensimmäinen sarake alkaa "a":lla seurattuna yhdellä tai useammalla numerolla, ja tulostaa toisen sarakkeen.

Integroimalla nämä ulkoiset komennot, Fish Shellin ohjelmoijat voivat hyödyntää täysin säännöllisten lausekkeiden tehoa monimutkaisten tekstimanipulaatiotehtävien suorittamiseen, parantaen shellin natiiveja kykyjä.
