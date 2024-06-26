---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:42.149344-07:00
description: "Miten: Bash itsess\xE4\xE4n ei sis\xE4ll\xE4 sis\xE4\xE4nrakennettuja\
  \ JSON-j\xE4sennysominaisuuksia, mutta `jq` on voimakas komentorivin JSON-prosessori,\
  \ joka t\xE4ytt\xE4\xE4 t\xE4m\xE4n\u2026"
lastmod: '2024-03-13T22:44:56.760091-06:00'
model: gpt-4-0125-preview
summary: "Bash itsess\xE4\xE4n ei sis\xE4ll\xE4 sis\xE4\xE4nrakennettuja JSON-j\xE4\
  sennysominaisuuksia, mutta `jq` on voimakas komentorivin JSON-prosessori, joka t\xE4\
  ytt\xE4\xE4 t\xE4m\xE4n aukon."
title: "Ty\xF6skentely JSON:n kanssa"
weight: 38
---

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
