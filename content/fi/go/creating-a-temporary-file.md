---
title:                "Väliaikaisen tiedoston luominen"
html_title:           "Go: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

"# Miksi luoda väliaikainen tiedosto?

Joskus ohjelmoinnissa on tarpeen luoda väliaikaisia tiedostoja, joita ei tarvita lopullisessa sovelluksessa. Tällaisia tiedostoja voidaan käyttää esimerkiksi väliaikaisina tallennuspaikkoina suurille tiedostoille tai väliaikaisina muuttujina erilaisissa algoritmeissa. Go:n avulla näiden väliaikaisten tiedostojen luominen on helppoa ja nopeaa.

## Miten tehdä

Väliaikaisen tiedoston luominen Go:n avulla onnistuu kätevästi "ioutil" -kirjaston avulla. Seuraavassa koodiesimerkissä näytetään, kuinka luodaan väliaikainen tiedosto ja kirjoitetaan siihen tekstiä:

```
package main

import (
    "fmt"
    "io/ioutil"
)

func main() {
    // Luodaan väliaikainen tiedosto
    tempFile, err := ioutil.TempFile("", "example")

    if err != nil {
        panic(err)
    }

    // Kirjoitetaan teksti tiedostoon
    _, err = tempFile.WriteString("Tämä on väliaikainen tiedosto")

    if err != nil {
        panic(err)
    }

    // Tulostetaan tiedoston nimi
    fmt.Println("Luotu väliaikainen tiedosto:", tempFile.Name())
}
```

Kun yllä oleva koodi suoritetaan, tulostetaan "Luotu väliaikainen tiedosto: /tmp/example342352234". Tässä nimi koostuu ensinmäisestä parametrista (tyhjästä merkkijonosta) ja toisesta parametrista (prefix asetettuna "example"). Tämä nimi on uniikki ja sitä voidaan käyttää tiedoston löytämiseen.

## Syvemmälle

Väliaikaisen tiedoston luomisessa on muutamia asioita, joita on hyvä tietää. Ensinnäkin, muistettavaa on että tiedosto pysyy niin kauan kuin ohjelma on käynnissä. Toiseksi, jos annat tyhjän merkkijonon ensimmäisenä parametrina, Go luo tiedoston oletushakemistoon (esim. Windows: C:\Users\nimi\AppData\Local\Temp/ ja Linux: /tmp/).

Väliaikainen tiedosto kannattaa myös aina poistaa, kun sitä ei enää tarvita, jotta se ei jää roskaksi järjestelmään. Tämä onnistuu "Clean" -metodilla, joka ottaa parametrinaan tiedoston nimen ja poistaa sen. Kannattaa myös huomioida, että Clean-metodia kutsutaan automaattisesti, kun ohjelma suljetaan.

## Katso myös

- Dokumentaatio: https://golang.org/pkg/io/ioutil/#TempFile
- Esimerkkejä: https://gobyexample.com/temporary-files