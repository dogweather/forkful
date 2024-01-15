---
title:                "Työskentely yaml:n kanssa"
html_title:           "Go: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi

Miksi YAML:ia kannattaa käyttää ohjelmoinnissa? No, vastaus on yksinkertainen: se on helppo tapa tallentaa ja siirtää tietoa eri muodoissa. YAML on myös ihmisen luettavissa oleva formaatti, mikä tekee siitä erityisen hyödyllisen selkeissä ja jäsennellyissä tiedostoissa.

## Kuinka Tehdä

YAML:n käyttöön pääsee helposti käsiksi käyttämällä Go:n "gopkg.in/yaml.v2" kirjastoa. Ensiksi tarvitsemme paketin tuomista varten import-lausekkeen:

```Go
import "gopkg.in/yaml.v2"
```

Seuraavaksi luo yksinkertainen YAML-tiedosto, joka sisältää muutamia tietoja:

```Go
var data = `
  language: Go
  version: current
  releaseDate: 2021-08-09
`
```

Tämän jälkeen voimme helposti muuntaa YAML-tiedoston map-objektiksi käyttämällä yaml.Unmarshal() -funktiota:

```Go
m := map[string]interface{}{}
err := yaml.Unmarshal([]byte(data), &m)
```

Nyt meillä on muuttuja "m", joka sisältää kaikki tiedostossa määritellyt avaimet ja arvot. Voimme käyttää niitä haluamallamme tavalla, esimerkiksi tulostamalla ne konsoliin:

```Go
fmt.Println(m["language"]) // tulostaa "Go"
fmt.Println(m["releaseDate"]) // tulostaa "2021-08-09"
```

## Syvempi Sukellus

Vaikka YAML on hyvin yksinkertainen ja ihmisen luettavissa oleva formaatti, sen käsittelyyn voi liittyä muutamia sudenkuoppia. Yksi tärkeimmistä on sykliset viittaukset, eli tilanne, jossa yksi objekti viittaa toiseen, ja tämä toinen viittaa taas takaisin ensimmäiseen. Tällaiset viittaukset voivat aiheuttaa loputtoman silmukan ja ohjelman kaatumisen. Siksi on tärkeää varmistaa, että YAML-tiedoston sisältöä käsitellessä otetaan huomioon myös tällaiset viittaukset.

## Katso Myös

- Go:n virallinen YAML-dokumentaatio: https://yaml.org/
- Gopkg.in/yaml.v2 -kirjaston GitHub-repositorio: https://github.com/go-yaml/yaml
- Stack Overflow -kysymykset ja vastaukset YAML:ista: https://stackoverflow.com/questions/tagged/yaml