---
title:                "Työskentely csv:n kanssa"
html_title:           "Go: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi

Miksi ihmiset käyttävät CSV-tiedostoja Go-ohjelmoinnissa? Yksinkertaisesti sanottuna, CSV (Comma-Separated Values) on yleinen tiedostomuoto tietojen tallentamiseen ja jakamiseen. Se on helppo lukea sekä ihmisille että tietokoneille, mikä tekee siitä suositun valinnan monissa sovelluksissa.

Jos haluat käsitellä suuria tietomääriä ja suorittaa monimutkaisia toimintoja, CSV-tiedostot voivat olla erittäin hyödyllisiä. Ne ovat myös käyttökelpoisia silloin, kun haluat tallentaa ja jakaa tietoja helposti eri ohjelmien välillä.

## Miten tehdä se

Go-kielellä CSV-tiedostojen käsittely on helppoa ja tehokasta. Voit lukea ja kirjoittaa CSV-tiedostoja käyttämällä sisäänrakennettuja paketteja, kuten "encoding/csv". Voit myös käyttää kolmannen osapuolen kirjastoja, kuten "go-csv", joka tarjoaa lisätoimintoja CSV-datan käsittelyyn.

Alla on esimerkki Go-ohjelmasta, joka käyttää "encoding/csv" pakettia CSV-tiedoston lukemiseen ja tulostaa sen sisällön konsolille:

````Go
package main

import (
    "encoding/csv"
    "fmt"
    "log"
    "os"
)

func main() {
    // Avaa CSV-tiedosto lukemista varten
    file, err := os.Open("data.csv")
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close()

    // Luo uusi CSV-lukija
    reader := csv.NewReader(file)

    // Lue tiedoston sisältö riveittäin
    for {
        // Lue rivi ja tallenna se taulukkoon
        record, err := reader.Read()
        if err == io.EOF { // Lopeta, kun saavutetaan tiedoston loppu
            break
        } else if err != nil {
            log.Fatal(err)
        }
        // Tulosta rivi konsolille
        fmt.Println(record)
    }
}
````

Esimerkkitiedoston "data.csv" sisältö:

```
Nimi,Ikä,Sukupuoli
Laura,25,Nainen
Matti,30,Mies
```

Ohjelman tuloste:

```
[Laura 25 Nainen]
[Matti 30 Mies]
```

Kuten näet, CSV-tiedoston lukeminen on melko yksinkertaista käyttämällä Go:n sisäänrakennettua pakettia. Voit myös käyttää samaa pakettia kirjoittamaan CSV-tiedostoja. Lisätietoja Go:n CSV-pakettien käytöstä löytyy virallisesta dokumentaatiosta.

## Syvään sukellus

CSV-tiedostot voivat tulla monimutkaisiksi, jos niissä on monenlaisia tiedonmuotoja tai suurta määrää rivejä. Tässä tapauksessa, voit käyttää pakettia "encoding/csv" Go:ssa monipuolisempien toimintojen suorittamiseen, esimerkiksi sarakekohdennuksen tai tiettyjen rajojen asettamiseen tiedoston lukuun.

Jos haluat oppia lisää CSV-tiedostojen käsittelystä Go:ssa, voit tutustua myös kolmannen osapuolen kirjastoihin, kuten "go-csv" tai "gota", jotka tarjoavat lisätoimintoja ja helpottavat monimutkaisempien CSV-tiedostojen käsittelyä.

## Katso myös

- [Go:n virallinen dokumentaatio CSV-paketeista](https://golang.org/pkg/encoding/csv/)
- [Go-csv -paketti GitHubissa](https://github.com/golang/csv)
- [Gota -paketti GitHubissa](https://github