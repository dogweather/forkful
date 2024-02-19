---
aliases:
- /fi/fish-shell/working-with-csv/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:55.508402-07:00
description: "CSV-tiedostojen (pilkuilla erotetut arvot) k\xE4sittely sis\xE4lt\xE4\
  \xE4 j\xE4sent\xE4misen, manipuloinnin ja datan tuottamisen taulukkomuodossa, jota\
  \ laajasti k\xE4ytet\xE4\xE4n\u2026"
lastmod: 2024-02-18 23:09:08.108790
model: gpt-4-0125-preview
summary: "CSV-tiedostojen (pilkuilla erotetut arvot) k\xE4sittely sis\xE4lt\xE4\xE4\
  \ j\xE4sent\xE4misen, manipuloinnin ja datan tuottamisen taulukkomuodossa, jota\
  \ laajasti k\xE4ytet\xE4\xE4n\u2026"
title: "Ty\xF6skentely CSV:n kanssa"
---

{{< edit_this_page >}}

## Mikä & Miksi?

CSV-tiedostojen (pilkuilla erotetut arvot) käsittely sisältää jäsentämisen, manipuloinnin ja datan tuottamisen taulukkomuodossa, jota laajasti käytetään datan vaihtoon sovellusten välillä. Ohjelmoijat suorittavat näitä toimintoja tehokkaasti käsitelläkseen ja analysoidakseen dataa, automatisoidakseen tehtäviä tai integroitavakseen muihin järjestelmiin.

## Kuinka:

Fish Shellillä itsellään ei ole sisäänrakennettuja funktioita nimenomaan CSV-manipulointiin suunniteltuna. Voit kuitenkin hyödyntää Unix-työkaluja kuten `awk`, `sed` ja `cut` perustoimintoihin tai käyttää erikoistuneita työkaluja kuten `csvkit` monimutkaisempiin tehtäviin.

### CSV-tiedoston lukeminen ja ensimmäisen sarakkeen tulostaminen:
Käytetään `cut`-komentoa ensimmäisen sarakkeen poimimiseen:
```fish
cut -d ',' -f1 data.csv
```
Näytekirjoitus:
```
Nimi
Alice
Bob
```

### CSV-rivien suodattaminen sarakkeen arvon perusteella:
Käyttäen `awk`:ia löytääkseen rivit, joissa toinen sarake vastaa "42":
```fish
awk -F, '$2 == "42" { print $0 }' data.csv
```
Näytekirjoitus:
```
Bob,42,Lontoo
```

### CSV-tiedoston muokkaaminen (esim. sarakkeen lisääminen):
Käyttäen `awk`:ia lisätäksesi sarakkeen, jossa on staattinen arvo "UusiSarake":
```fish
awk -F, 'BEGIN {OFS=","} {print $0,"UusiSarake"}' data.csv > muokattu.csv
```
Näyte `muokattu.csv`-tulostuksessa:
```
Nimi,Ikä,Kaupunki,UusiSarake
Alice,30,New York,UusiSarake
Bob,42,Lontoo,UusiSarake
```

### `csvkit`in käyttö monimutkaisempiin toimintoihin:
Varmista ensin, että sinulla on `csvkit` asennettuna. Jos ei, asenna se käyttäen pip: `pip install csvkit`.

**CSV-tiedoston muuntaminen JSON-muotoon:**
```fish
csvjson data.csv > data.json
```
Näyte `data.json`-tulostuksessa:
```json
[{"Nimi":"Alice","Ikä":"30","Kaupunki":"New York"},{"Nimi":"Bob","Ikä":"42","Kaupunki":"Lontoo"}]
```

**Suodattaminen käyttäen `csvkit`in `csvgrep`:**
```fish
csvgrep -c 2 -m 42 data.csv
```
Tämä komento toistaa suodatustehtävän mutta käyttäen `csvkit`:iä, kohdistuen sarakkeeseen 2 arvolla "42".

Yhteenvetona, vaikka Fish Shell itsessään ei ehkä tarjoa suoraa CSV-manipulointikyvykkyyttä, sen saumaton integraatio Unix-työkalujen kanssa ja työkalujen kuten `csvkit` saatavuus tarjoavat tehokkaita vaihtoehtoja työskennellä CSV-tiedostojen kanssa.
