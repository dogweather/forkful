---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:10.063566-07:00
description: "Kuinka: **CSV-tiedoston lukeminen rivi rivilt\xE4**."
lastmod: '2024-03-13T22:44:56.761096-06:00'
model: gpt-4-0125-preview
summary: "**CSV-tiedoston lukeminen rivi rivilt\xE4**."
title: "Ty\xF6skentely CSV:n kanssa"
weight: 37
---

## Kuinka:
**CSV-tiedoston lukeminen rivi riviltä**

```bash
while IFS=, read -r column1 column2 column3
do
  echo "Sarake 1: $column1, Sarake 2: $column2, Sarake 3: $column3"
done < sample.csv
```

*Esimerkkituloste:*

```
Sarake 1: id, Sarake 2: nimi, Sarake 3: sähköposti
...
```

**CSV-rivien suodattaminen ehdolla**

Käyttämällä `awk`-komentoa, voit helposti suodattaa rivejä. Esimerkiksi löytääksesi rivit, joissa toinen sarake on "Alice":

```bash
awk -F, '$2 == "Alice" { print $0 }' sample.csv
```

**Sarakkeen arvon muuttaminen**

Muuttaaksesi toisen sarakkeen isoiksi kirjaimiksi:

```bash
awk -F, 'BEGIN {OFS=",";} { $2 = toupper($2); print $0; }' sample.csv
```

**CSV-tiedoston lajittelu sarakkeen perusteella**

Voit lajitella CSV-tiedoston perustuen esimerkiksi kolmanteen sarakkeeseen (numeerisesti):

```bash
sort -t, -k3,3n sample.csv
```

**`csvkit`-työkalun käyttö monimutkaisempiin tehtäviin**

`csvkit` on komentorivin työkalujen sarja, joka on suunniteltu muuntamaan ja käsittelemään CSV-tiedostoja. Sen voi asentaa pip:n kautta.

Muuttaaksesi JSON-tiedoston CSV-muotoon:

```bash
in2csv data.json > data.csv
```

Kyselyjen suorittaminen CSV-tiedostossa SQL:llä:

```bash
csvsql --query "SELECT nimi FROM sample WHERE id = 10" sample.csv
```

*Huom: `csvkit` vaatii Pythonin ja sen voi asentaa käyttämällä `pip install csvkit`.*
