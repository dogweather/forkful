---
title:                "Go: Alimerkkijonojen erottaminen"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi?

Joskus koodin luvattaminen tai analysointi vaatii tiettyjen merkkijonojen erottamista suuremmasta tekstikokonaisuudesta. Tämä voidaan tehdä luomalla osatekijäkohtia tai tekstipaloja, ja tämä prosessi tunnetaan substraktioina.

## Miten tehdä?

Käytännössä Go-kielellä substraktiot voidaan tehdä käyttämällä `strings`-pakettia ja sen `Substring`-toimintoa. Seuraavassa esimerkissä otetaan syötteenä olevasta tekstistä osatekijät aikavälillä 0-5 ja tulostetaan ne näytölle.

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    text := "Tässä on esimerkkiteksti"
    substr := text[0:5] // otetaan osatekijät

    fmt.Println(substr) // tulostetaan "Tässä"
}
```

Tämä toiminto on erityisen hyödyllinen, jos haluat esimerkiksi tarkistaa, onko tietyllä merkkijonolla tietyn alku tai loppu.

## Syvällisempi sukellus

Go-kielessä `Substring`-toimintoa voidaan käyttää myös monimutkaisempiin substraktiioihin. Voit esimerkiksi määrittää muuttujan, joka sisältää haluamasi alun ja lopun indeksit, ja käyttää näitä arvoja `Substring`-toiminnossa.

On myös mahdollista käyttää `Substring` yhdistettynä muihin `strings`-paketin toimintoihin, kuten `IndexOf`, jotta voit löytää halutun merkkijonon tietyssä osassa tekstiä.

## Katso myös

- `strings`-paketti Go-kielessä: https://golang.org/pkg/strings/
- Video selitys Go:sta ja substringien käytöstä: https://www.youtube.com/watch?v=_P9LzCvC-mM