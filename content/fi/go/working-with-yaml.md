---
title:                "Go: Yamlin käyttöohjeet"
simple_title:         "Yamlin käyttöohjeet"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/working-with-yaml.md"
---

{{< edit_this_page >}}

(# Miksi)

Miksi YAML:in kanssa kannattaa työskennellä Go-ohjelmointiympäristössä?

YAML, eli "Yet Another Markup Language", on helppolukuinen tiedostomuoto, jonka avulla voidaan tallentaa ja jakaa tietoja rakenteellisesti. Se on erityisen hyödyllinen ohjelmointiympäristöissä, kuten Go, sillä se mahdollistaa helpon tiedon tallentamisen ja käytön eri sovelluksissa.

(# Kuinka)

Kuinka käyttää YAML:ia Go-ohjelmoinnissa?

YAML:ia varten on Go-kielessä valmiiksi määritelty paketti "gopkg.in/yaml.v2" joka tarjoaa kätevät toiminnot tiedostojen lukemiseen ja kirjoittamiseen. Seuraavassa on esimerkkikoodi, miten tietoa voidaan lukea YAML-tiedostosta ja tulostaa konsoliin:

```Go
package main

import (
    "fmt"
    "io/ioutil"
    "gopkg.in/yaml.v2"
)

type ExampleData struct {
    Name string `yaml:"name"`
    Age int `yaml:"age"`
}

func main() {
    // luetaan YAML-tiedosto
    data, err := ioutil.ReadFile("example.yaml")
    if err != nil {
        panic(err)
    }

    // initialisoidaan datarakenteen muuttuja
    var sampleData ExampleData

    // muunnetaan YAML-tiedoston sisältö datarakenteeksi
    err = yaml.Unmarshal(data, &sampleData)
    if err != nil {
        panic(err)
    }

    // tulostetaan data konsoliin
    fmt.Println("Nimi:", sampleData.Name)
    fmt.Println("Ikä:", sampleData.Age)
}
```

(# Pohjanoteeraus)

Miten työskennellä syvällisemmin YAML:in kanssa?

YAML muodostuu eri osista, kuten otsikot, kentät ja listat, jotka voidaan tulkita vastaaviksi rakenteiksi Go-kielessä. On tärkeä ymmärtää nämä eri osat ja niiden käyttö, jotta YAML-tiedostojen lukeminen ja kirjoittaminen sujuu hyvin. Lisätietoja löytyy Go:n paketin dokumentaatiosta.

(# Katso myös)

- [Go YAML -paketti](https://pkg.go.dev/gopkg.in/yaml.v2)
- [Go YAML -paketin dokumentaatio](https://pkg.go.dev/gopkg.in/yaml.v2#Unmarshaler.UnmarshalYAML)
- [YAML spekifikaatio](https://yaml.org/spec/)