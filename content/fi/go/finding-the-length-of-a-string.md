---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "Go: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Merkkijonon pituuden löytäminen tarkoittaa yksinkertaisesti merkkijonon sisältämien merkkien lukumäärän laskemista. Tämä on tärkeää ohjelmoinnissa esimerkiksi, kun halutaan tarkistaa, että käyttäjän antama syöte ei ylitä ennalta määritettyä rajaa.

## Miten:
Seuraavissa esimerkeissä näytämme, miten voit laskea merkkijonon pituuden käyttämällä Go:n sisäänrakennettua ```len()``` -funktiota. 

Esimerkki 1:
```Go
var s = "Tämä on esimerkki merkkijonosta"
fmt.Println(len(s))
```

Tuloste:
```
35
```

Esimerkki 2:
```Go
var s = ""
len := len(s)
fmt.Println("Tämän merkkijonon pituus on:", len)
```

Tuloste:
```
Tämän merkkijonon pituus on: 0
```

## Syvemmälle:
Merkkijonon pituuden laskeminen on nykyään helppoa kiitos modernien ohjelmointikielten, kuten Go:n, sisäänrakennetun ```len()``` -funktion. Aikaisemmissa kielissä, kuten C:ssä, tämä vaati enemmän ohjelmointia ja muuttujien käsittelyä. Go:n lisäksi myös monilla muilla kielillä on vastaavia sisäänrakennettuja toimintoja merkkijonojen pituuden laskemiseen, esimerkiksi JavaScriptin ```length``` -ominaisuus.

## Katso myös:
Tässä muutamia hyödyllisiä linkkejä liittyen merkkijonon pituuden laskemiseen ja Go-ohjelmointiin:

- [Go:n virallinen dokumentaatio merkkijonon pituuden laskemisesta](https://golang.org/pkg/strings/#Count)
- [Kattava artikkeli merkkijonon pituuden laskemisesta Go-kielellä](https://www.calhoun.io/finding-the-length-of-a-string-in-go/)
- [Esimerkkejä erilaisten syötteiden käsittelystä Go:ssa](https://stackoverflow.com/questions/44420152/how-to-process-input-using-go)