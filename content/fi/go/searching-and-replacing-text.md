---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Go: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi etsiä ja korvata tekstiä Go-kielellä? Yksinkertaisesti sanottuna, tekstien etsiminen ja korvaaminen on erittäin hyödyllinen toiminto, jota voi tarvita koodauksessa. Se voi säästää aikaa ja vaivaa ja auttaa pitämään koodisi järjesteltynä ja tehokkaana.

## Miten

Etsi ja korvaa -toiminnon toteuttaminen Go-kielellä ei ole vaikeaa. Voit käyttää sisäänrakennettua replace-funktiota, joka löytyy strings-paketista. Tämä funktio ottaa kolme parametria: teksti, jota haluat etsiä, korvaava teksti ja alkuperäinen teksti, jossa haluat suorittaa korvauksen.

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    text := "Tervetuloa Go-kieleen!"
    newText := strings.Replace(text, "Tervetuloa", "Hei", 1)
    fmt.Println(newText)
}
```

Tässä esimerkissä käytämme replace-funktiota korvaamaan sanan "Tervetuloa" sanalla "Hei". Kolmas parametri "1" tarkoittaa, että korvaus tehdään vain ensimmäiseen esiintymään. Tulostus olisi "Hei Go-kieleen!".

Voit myös käyttää strings.ReplaceAll-funktiota, jos haluat korvata kaikki esiintymät.

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    text := "Tervetuloa Tervetuloa Tervetuloa!"
    newText := strings.ReplaceAll(text, "Tervetuloa", "Hei")
    fmt.Println(newText)
}
```

Tämä tulostaisi "Hei Hei Hei!".

## Syvempi sukellus

Go tarjoaa myös muita vaihtoehtoja tekstien etsimiseen ja korvaamiseen. Voit esimerkiksi käyttää regex-käyttöliittymää käyttämällä regexp-pakettia. Tämä mahdollistaa monimutkaisempien korvausten ja hakuoperaatioiden suorittamisen.

```Go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    text := "Auto, moottoripyörä, polkupyörä"
    re := regexp.MustCompile("moottoripyörä")
    newText := re.ReplaceAllString(text, "vene")
    fmt.Println(newText)
}
```

Tässä esimerkissä käytämme regexp-pakettia etsimään sanaa "moottoripyörä" ja korvaamaan sen sanalla "vene". Tulostus olisi "Auto, vene, polkupyörä".

On myös muita usein käytettyjä kirjastoja, kuten text/template ja strings.Builder, jotka tarjoavat lisää vaihtoehtoja tekstien etsimiseen ja korvaamiseen.

## Katso myös

- [Go:n virallinen dokumentaatio tekstien korvaamisesta](https://golang.org/pkg/strings/#Replace)
- [Go:n regexp-paketti](https://golang.org/pkg/regexp/)
- [Go:n strings.Builder-paketti](https://golang.org/pkg/strings/#Builder)