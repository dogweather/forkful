---
title:                "Tiedoston kirjoittaminen"
html_title:           "Go: Tiedoston kirjoittaminen"
simple_title:         "Tiedoston kirjoittaminen"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen on yksi tärkeimmistä toiminnoista ohjelmoinnissa, ja tekstitiedostot ovat yleinen tapa tallentaa ja lukea tietoa. Siksi on tärkeää tietää, miten kirjoitat ja hallitset tekstitiedostoja Go-kielellä.

## Miten

Go-kielessä tekstitiedostojen kirjoittaminen ja lukeminen on hyvin yksinkertaista. Se tapahtuu käyttämällä `io/ioutil` -pakettia.

Aluksi sinun täytyy tuoda `io/ioutil`-paketti koodiisi: 
```Go
import "io/ioutil"
```

Kirjoittaessasi tekstitiedostoa, sinun täytyy määrittää tiedoston polku ja sisältö, jonka haluat tallentaa: 
```Go
file := []byte("Tämä on esimerkkiteksti.")
path := "/polku/tiedostoon/tekstitiedosto.txt"
```

Sitten voit käyttää `ioutil.WriteFile()`-funktiota tallentaaksesi tiedoston sisällön haluamallesi polulle: 
```Go
err := ioutil.WriteFile(path, file, 0644)
if err != nil {
    panic("Tiedoston tallentaminen epäonnistui.")
}
```

Voit myös käyttää `ioutil.WriteFile()`-funktiota lukeaksesi olemassa olevan tekstitiedoston sisällön: 
```Go
file, err := ioutil.ReadFile(path)
if err != nil {
    panic("Tiedoston lukeminen epäonnistui.")
}

fmt.Print(string(file)) // tulostaa tekstitiedoston sisällön konsolille
```

## Deep Dive

Go-kielellä tekstitiedostojen kirjoittaminen tapahtuu käyttämällä `io/ioutil`-pakettia, mutta voit myös käyttää muita paketteja, kuten `os` ja `bufio`.

`os`-pakettia voidaan käyttää luomaan uusi tiedosto ja hallitsemaan tiedoston järjestelmätietoja, kuten oikeuksia ja päiväystä. `bufio`-pakettia voidaan käyttää myös lukemaan ja kirjoittamaan tiedoston sisältöä bitteinä.

On myös tärkeää muistaa, että tekstitiedostoilla on erilaisia muotoilu- ja koodaustapoja, kuten UTF-8, ASCII ja Unicode. Siksi on tärkeää varmistaa, että käytät oikeita muotoiluja ja koodaustapoja tekstitiedostoja lukiessasi ja kirjoittaessasi.

## Katso myös

- [Go-standardikirjaston dokumentaatio - io/ioutil](https://golang.org/pkg/io/ioutil/)
- [Go-standardikirjaston dokumentaatio - os](https://golang.org/pkg/os/)
- [Go-standardikirjaston dokumentaatio - bufio](https://golang.org/pkg/bufio/)