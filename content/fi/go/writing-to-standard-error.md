---
title:                "Kirjoittaminen standardivirheeseen"
html_title:           "Go: Kirjoittaminen standardivirheeseen"
simple_title:         "Kirjoittaminen standardivirheeseen"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Oletko koskaan kohdannut virheitä ohjelmoitaessa, mutta et tiennyt miten käsitellä niitä tai mitä tietoja tulostaa? Tässä artikkelissa opimme kirjoittamaan virheet standardi virhe-tulostuskanavaan, jotta voit saada lisätietoja virheistä ohjelmasi suorittamisen aikana.

## Miten

Kirjoittaminen standardi virhe-tulostuskanavaan Go-kielellä on helppoa ja nopeaa. Kaikki mitä tarvitset on käyttää `os.Stderr` -pakettia ja `fmt.Fprintf()` -funktiota. Katsotaanpa käytännön esimerkin avulla, miten tämä tapahtuu:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Simuloidaan virheellistä tilannetta laskemalla nollalla
    result, err := divide(10, 0)
    
    // Tarkistetaan, onko virhe olemassa ja kirjoitetaan se standardi virhe-tulostuskanavaan
    if err != nil {
        fmt.Fprintf(os.Stderr, "Virhe: %v\n", err)
    }
    
    fmt.Println(result)
}

// Funktio jakaa x:n y:llä ja palauttaa mahdollisen virheen
func divide(x, y float64) (float64, error) {
    if y == 0 {
        return 0, fmt.Errorf("ei voi jakaa nollalla")
    }
    
    return x / y, nil
}
```

Tulostusohjeiden lisäksi voit myös käyttää `log` -pakettia, joka tarjoaa useita käteviä funktioita virheiden kirjaamiseen standardi virhe-tulostuskanavaan.

```Go
package main

import (
    "log"
)

func main() {
    // Simuloidaan virheellistä tilannetta
    err := doSomething()
    
    // Kirjoitetaan virhe standardi virhe-tulostuskanavaan
    log.Println(err)
}

// Tämä funktio palauttaa virheen ilman tietoja
func doSomething() error {
    return errors.New("jotain meni pieleen")
}
```

Nämä esimerkit näyttävät kuinka helppoa ja hyödyllistä on kirjoittaa virheitä standardi virhe-tulostuskanavaan. Voit myös ottaa huomioon kontekstin ja lisätä muuta tietoa virheiden kirjoittamiseen, kuten aikaleiman tai ohjelman nimen.

## Syvällinen sukellus

Kirjoittaminen standardi virhe-tulostuskanavaan ei ole aina ensisijainen vaihtoehto virheiden käsittelyssä, mutta se on hyödyllinen työkalu tietyissä tilanteissa. Tämä tulostuskanava on hyödyllinen, jos haluat saada lisätietoa ohjelmasi virheistä suorituksen aikana. Kuitenkin, jos haluat käsitellä virheitä tietyllä tavalla, esimerkiksi tallentamalla ne tiedostoon, on parempi käyttää `log` -pakettia.

Jos haluat lisätietoa virheiden käsittelystä Go-kielellä, voit lukea virallisen dokkumentaation tästä aiheesta: https://golang.org/pkg/os/#Fprintf ja https://golang.org/pkg/log/.

## Katso myös

- https://golang.org/pkg/os/#Stderr
- https://golang.org/pkg/log/