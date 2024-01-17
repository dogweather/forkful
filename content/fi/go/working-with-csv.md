---
title:                "Työskentely csv-tiedostojen kanssa"
html_title:           "Go: Työskentely csv-tiedostojen kanssa"
simple_title:         "Työskentely csv-tiedostojen kanssa"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/working-with-csv.md"
---

{{< edit_this_page >}}

## Mikä? & Miksi?
CSV (Comma-Separated Values) on tiedostomuoto, joka tallentaa taulukkomuotoisia tietoja pilkuilla eroteltuina arvoina. Ohjelmoijat käyttävät CSV:tä usein tiedonsiirtoon tietokantojen ja erilaisten ohjelmistojen välillä.

## Miten:
Go-kielellä CSV:n käsittely on helppoa ja tehokasta. Alla on kaksi esimerkkiä CSV-tiedoston lukemisesta ja siitä saatavan datan käsittelemisestä.

```Go
// Esimerkki 1: Lukeminen ja tulostaminen

file, err := os.Open("data.csv") // avataan CSV-tiedosto
if err != nil {
    // virheenkäsittely
}
defer file.Close() // tiedosto sulkeutuu automaattisesti

reader := csv.NewReader(file)
records, err := reader.ReadAll() // luetaan tiedoston kaikki rivit
if err != nil {
    // virheenkäsittely
}

fmt.Println(records) // tulostetaan taulukko

/*
Tuloste:
[
  [nimi ikä kaupunki]
  [Matti 30 Helsinki]
  [Anna 25 Tampere]
  [Teemu 35 Oulu]
  ...
]
*/
```

```Go
// Esimerkki 2: Etsiminen ja muokkaaminen

records := [][]string{
  {"nimi", "ikä", "kaupunki"},
  {"Matti", "30", "Helsinki"},
  {"Anna", "25", "Tampere"},
  {"Teemu", "35", "Oulu"},
  ...
}

var result string
for _, record := range records {
  if record[0] == "Anna" { // etsitään riviltä nimi "Anna"
    result = record[2] // tallennetaan riviltä kaupunki "Tampere"
    break // lopetetaan silmukka
  }
}

fmt.Println(result) // tulostetaan "Tampere"
```

## Syvemmälle:
CSV oli alunperin käytössä pääasiassa laskentataulukoissa, mutta nykyään sitä käytetään monissa muissakin järjestelmissä. Vaihtoehtoina CSV:lle ovat esimerkiksi JSON ja XML, mutta CSV on edelleen suosittu erityisesti yksinkertaisissa ja selkeissä datarakenteissa. Go:n sisäänrakennettu "encoding/csv" -kirjasto tarjoaa helpon ja tehokkaan tavan käsitellä CSV-tiedostoja.

## Katso myös:
- [Go:n virallinen dokumentaatio CSV-käsittelyyn](https://golang.org/pkg/encoding/csv/)
- [CSV-tiedostomuodon yleiskatsaus](https://en.wikipedia.org/wiki/Comma-separated_values)