---
title:                "Työskentely JSON:n kanssa"
aliases:
- /fi/bash/working-with-json/
date:                  2024-02-03T19:21:42.149344-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely JSON:n kanssa"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?
JSONin käsittely Bash-ohjelmoinnissa sisältää JSON-tietojen jäsentämisen, poimimisen ja manipuloinnin suoraan komentoriviltä. Ohjelmoijat tekevät näin usein integroidakseen kuoriskriptejä saumattomasti web-API:en ja modernien tietovaihtoformaatien kanssa, tehden Bash-skriptauksesta voimakkaampaa ja relevantimpaa JSON-painotteisessa ekosysteemissä.

## Miten:
Bash itsessään ei sisällä sisäänrakennettuja JSON-jäsennysominaisuuksia, mutta `jq` on voimakas komentorivin JSON-prosessori, joka täyttää tämän aukon. Näin voit käyttää sitä:

**JSON-tiedoston lukeminen:**

Esimerkki `data.json`:
```json
{
  "name": "Jane Doe",
  "email": "jane@example.com",
  "location": {
    "city": "New York",
    "country": "USA"
  }
}
```

Luetaan ja poimitaan nimi JSON-tiedostosta:
```bash
jq '.name' data.json
```
Tuloste:
```
"Jane Doe"
```

**JSON-tietojen muokkaaminen:**

Kaupungin päivittämiseksi "Los Angelesiksi" ja kirjoitetaan takaisin tiedostoon:
```bash
jq '.location.city = "Los Angeles"' data.json > temp.json && mv temp.json data.json
```

**JSONin jäsentäminen muuttujasta:**

Jos sinulla on JSON Bash-muuttujassa, `jq` voi silti käsitellä sitä:
```bash
json_string='{"name": "John Doe", "email": "john@example.com"}'
echo $json_string | jq '.name'
```
Tuloste:
```
"John Doe"
```

**Taulukoiden kanssa työskentely:**

Olettaen, että sinulla on taulukko kohteita JSONissa:
```json
{
  "items": ["apple", "banana", "cherry"]
}
```

Toisen kohteen (indeksointi alkaa 0:sta) poimimiseksi:
```bash
jq '.items[1]' data.json
```
Tuloste:
```
"banana"
```

Monimutkaisempien operaatioiden ja suodattimien osalta `jq`:lla on kattava manuaali ja tutoriaalit saatavilla verkossa, tehden siitä monipuolisen työkalun kaikkiin Bash/JSON-tarpeisiisi.
