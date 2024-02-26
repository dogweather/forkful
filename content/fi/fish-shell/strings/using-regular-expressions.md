---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:34.842112-07:00
description: "S\xE4\xE4nn\xF6lliset lausekkeet (regex) Fish Shelliss\xE4 mahdollistavat\
  \ merkkijonojen etsimisen, vastaavuuksien l\xF6yt\xE4misen ja manipuloinnin tiettyjen\
  \ mallien\u2026"
lastmod: '2024-02-25T18:49:53.889832-07:00'
model: gpt-4-0125-preview
summary: "S\xE4\xE4nn\xF6lliset lausekkeet (regex) Fish Shelliss\xE4 mahdollistavat\
  \ merkkijonojen etsimisen, vastaavuuksien l\xF6yt\xE4misen ja manipuloinnin tiettyjen\
  \ mallien\u2026"
title: "S\xE4\xE4nn\xF6llisten lausekkeiden k\xE4ytt\xF6"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Säännölliset lausekkeet (regex) Fish Shellissä mahdollistavat merkkijonojen etsimisen, vastaavuuksien löytämisen ja manipuloinnin tiettyjen mallien perusteella. Ohjelmoijat käyttävät regexiä tehtäviin kuten syötteen validointi, jäsentäminen ja tekstin käsittely, koska se tarjoaa tiiviin ja tehokkaan tavan määritellä monimutkaisia tekstimalleja.

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
