---
title:    "Go: Merkkijonon isolla kirjoittaminen"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuttaa merkkijonon ensimmäisen kirjaimen isoksi Go-ohjelmointikielellä? Yksi syy voisi olla tiedon oikea muotoilu esimerkiksi käyttäjän syötteistä. Tämä voidaan helposti saavuttaa muuttamalla merkkijonon ensimmäinen kirjain isoksi.

## Kuinka tehdä

Käytännössä tämä tarkoittaa merkkijonon ensimmäisen kirjaimen muuttamista isoksi suoraan koodissa. Katso alla oleva koodiesimerkki ja sen tuottama tulostus, jotta ymmärrät paremmin miten tämä onnistuu Go-ohjelmointikielellä.

```Go
package main

import "fmt"
import "strings"

func capitalizeString(str string) string {
	return strings.ToUpper(str[:1]) + str[1:]
}

func main() {
	str := "hello world"

	fmt.Println("Ensimmäinen kirjain isona: " + capitalizeString(str))
}

```

Tulostus:
```
Ensimmäinen kirjain isona: Hello world
```

Tässä koodiesimerkissä käytetään Go:n sisäänrakennettua "strings" -pakettia, joka sisältää hyödyllisiä toimintoja merkkijonojen käsittelyyn. Tässä tapauksessa käytämme "ToUpper" -funktiota, joka muuttaa merkkijonon kirjaimet isoiksi. Lisäksi "capitalizeString" -funktiossa käytetään "[:1]" ja "[1:]" -merkintöjä, jotka tarkoittavat merkkijonon ensimmäistä ja sen jälkeisiä kirjaimia.

## Syvälle sukeltaminen

Tämän yksinkertaisen tehtävän taustalla on kuitenkin tärkeämpiä syitä. Go:n sisäänrakennettuja merkkijonofunktioita kannattaa käyttää, sillä ne ovat suorituskykyisiä ja tehokkaita toisin kuin esimerkiksi perinteisiä "for"-silmukoita. Muistinvarainen muunnos suoritetaan suoraan lähtömuuttujaan, eikä uusia merkkijono-olioita luoda tarpeettomasti. Tämä johtaa nopeampaan ja vähemmän resursseja käyttävään koodiin.

## Katso myös

- [Go - viralliset dokumentaatiot](https://golang.org/doc/)
- [Huomattavien kontrastien poistaminen teksteistä Go-ohjelmointikielellä](https://blog.golang.org/strings)
- [Merkkijonofunktioiden benchmark-testit](https://gist.github.com/chidiwilliams/5121778)