---
title:                "Go: Tilapäistiedoston luominen"
simple_title:         "Tilapäistiedoston luominen"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi luoda tilapäisiä tiedostoja?

Luodaanpa tilapäinen tiedosto Go-ohjelmoinnin kautta. Usein ohjelmia kehitettäessä, tarvitaan väliaikainen tiedosto johon tallentaa tietoja. Tämä on hyödyllistä esimerkiksi testauksessa, kun tarvitaan väliaikaista tallennustilaa eri vaiheiden suorittamiseen.

## Kuinka luoda tilapäinen tiedosto

Loikataan suoraan esimerkin pariin. Alla oleva esimerkki luo väliaikaisen tiedoston nimeltä "testi.txt" ja tallentaa siihen tekstin "Tämä on tilapäinen tiedosto." Sen jälkeen tiedosto suljetaan ja poistetaan:

```Go
package main

import (
    "fmt"
    "io/ioutil"
    "os"
)

func main() {
    // Luodaan tilapäinen tiedosto
    f, err := ioutil.TempFile("", "testi.txt")

    if err != nil {
        fmt.Println(err)
        return
    }

    // Kirjoitetaan teksti tiedostoon
    text := "Tämä on tilapäinen tiedosto."
    f.Write([]byte(text))

    // Suljetaan ja poistetaan tiedosto
    f.Close()
    os.Remove(f.Name())
}
```

Yllä olevan esimerkin suorittamisen jälkeen, koodin pitäisi luoda ja poistaa tilapäinen tiedosto nimeltä "testi.txt". Voit tarkistaa tämän esimerkiksi avaamalla tiedostonhallinnan ja etsimällä tiedoston nimellä "testi.txt".

## Syväsukellus tilapäisten tiedostojen luomiseen

Tilapäisten tiedostojen luominen voi olla hyödyllistä myös silloin kun halutaan tallentaa väliaikaista tietoa tai piilottaa tietoja muilta käyttäjiltä. Esimerkiksi Go-ohjelma voi luoda tilapäisen tiedoston, tallentaa siihen arkaluonteista tietoa ja poistaa sen lopuksi, jolloin tieto ei jää muiden nähtäväksi.

Tilapäisten tiedostojen luomiseen on myös olemassa erilaisia vaihtoehtoja ja parametreja, jotka voivat vaikuttaa tiedoston luontiin ja poistamiseen. Lisätietoa näistä vaihtoehdoista löytyy Go-kielen virallisesta dokumentaatiosta.

## Katso myös

- [Go-kielen virallinen dokumentaatio](https://golang.org/doc/)
- [Syvä sukellus asiakkaiden luomiseen Go:lla](https://blog.suzannechurchill.com/golang-file-server/)