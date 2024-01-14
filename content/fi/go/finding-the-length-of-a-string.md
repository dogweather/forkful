---
title:    "Go: Merkkijonon pituuden löytäminen"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi löytää merkkijonon pituus on tärkeää Go-ohjelmoinnissa? Löydämme vastauksen seuraavasti:

1. Monet ohjelmoinnin ongelmat vaativat merkkijonojen käsittelyä, ja merkkijonon pituus on tärkeä tieto monissa tilanteissa.
2. Go-ohjelmoinnissa on useita tapoja löytää merkkijonon pituus, ja tämä taito on hyödyllinen keskeisissä ohjelmointitaitoissa.

## Kuinka

Voit löytää helposti merkkijonon pituuden Go-kielellä käyttämällä `len()` -funktiota. Seuraavassa on yksinkertainen esimerkki:

```
Go package main

import "fmt"

func main() {
	str := "Terve maailma!"
	fmt.Println("Merkkijonon 'str' pituus on", len(str))
}
```

Output:
```
Merkkijonon 'str' pituus on 14
```

Voit myös käydä läpi merkkijonon jokaisen merkin käyttämällä `range`-silmukkaa ja laskemalla iterointien määrän. Tässä on toinen esimerkki:

```
Go package main

import "fmt"

func main() {
	str := "Hei!"
	count := 0
	for range str {
		count++
	}
	fmt.Println("Merkkijonon pituus on", count)
}
```

Output:
```
Merkkijonon pituus on 4
```

## Syvällinen sukellus

Merkkijonojen käsittelyyn kuuluu usein myös merkkien kasittely, esimerkiksi erottelemalla merkkijono osiksi käyttämällä `strings.Split()` -funktiota. Tämä saattaa vaatia merkkijonon pituuden tarkkaa laskentaa ja hyvää ymmärrystä merkkijonojen käsittelystä.

Toinen merkittävä tekijä, joka vaikuttaa merkkijonon pituuslaskentaan, on merkkien koodauksen tyyppi, kuten Unicode, joka käyttää useita tavuja yhdessä merkissä. Tästä syystä merkkijonon pituuden laskeminen voi olla monimutkaisempaa ja vaatia lisätyötä.

## Katso myös

- [Golang Suomi](https://golang.fi/)
- [Go-kielen merkkijonon käsittely](https://medium.com/rungo/string-data-type-in-go-8f01a3b4b14b)
- [Go-kielen dokumentaatio merkkijonojen käsittelystä](https://godoc.org/strings)