---
title:                "Go: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit löytää merkkijonon pituuden? Monissa ohjelmoinnin tehtävissä on tarpeen tietää, kuinka monta merkkiä merkkijonossa on. Tämä tieto voi auttaa esimerkiksi merkkijonon käsittelyssä tai sen muokkaamisessa.

## Miten

Merkkijonon pituuden löytäminen Go-kielellä on yksinkertaista käyttämällä `len()` -funktiota. Oletetaan, että haluat tietää, kuinka monta merkkiä on "Hei maailma!", voit käyttää seuraavaa koodia:

```Go
s := "Hei maailma!"
pituus := len(s)
fmt.Println(pituus)
```

Tämä tulostaa konsoliin luvun 12, mikä vastaa merkkijonon "Hei maailma!" pituutta. Voit myös käyttää `len()` -funktiota suoraan ilman väliaikaista muuttujaa:

```Go
fmt.Println(len("Hei maailma!"))
```

Tämä tuottaa saman tuloksen.

## Syvemmälle

Go-kielessä merkkijonon pituus lasketaan tavuina eikä merkkeinä. Tämä tarkoittaa, että jotkut merkit, kuten ääkköset, voivat vaatia useamman tavun tallentamiseen ja siten vaikuttaa merkkijonon pituuteen. Lisäksi `len()`-funktio voi laskea myös muiden tietotyyppien, kuten taulukoiden ja karttojen, pituuden.

## Katso myös

- Merkkijonojen käsittely Go-kielessä: https://www.golang-book.com/books/intro/4#section3
- Go-kielen dokumentaatio: https://golang.org/doc/
- Go-kielessä käytettävät tietotyypit: https://golang.org/ref/spec#Types