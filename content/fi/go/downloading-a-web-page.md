---
title:                "Go: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi

Monissa sovelluksissa on tarve ladata ja käsitellä web-sivuja, esimerkiksi webskraping tai tiedon keräämiseksi. Tässä blogipostauksessa käymme läpi kuinka voit ladata web-sivuja Go-kielellä.

## Miten

Go-kielellä web-sivun lataaminen on yksinkertaista käyttäen `net/http` pakettia. Käytännössä käytämme `Get` funktiota joka ottaa vastaan URL:n parametrina ja palauttaa vastauksen ja mahdollisten virheiden lisäksi. Alla on esimerkkejä lataamisesta ja vastauksen käsittelystä.

```Go
// Importoi paketti
import "net/http"

// Luodaan uusi HTTP pyyntö ja tarkistetaan mahdolliset virheet
resp, err := http.Get("https://www.example.com")
if err != nil {
    // Käsittelyä virheille
    fmt.Println("Virhe latauksessa:", err)
}
defer resp.Body.Close()

// Luetaan vastaus ja tulostetaan se konsoliin
body, err := ioutil.ReadAll(resp.Body)
if err != nil {
    fmt.Println("Virhe luettaessa vastausta:", err)
}
fmt.Println(string(body))
```

Käytännössä voimme myös haluta ladata vain tietyn tyyppisiä tiedostoja tai käsitellä vastausta eri tavalla. Esimerkiksi alla olevassa koodissa käytetään `File` nimeä URL:n perässä jotta vain tiedostot ladataan.

```Go
// Luodaan uusi pyyntö ja määritetään tiedoston nimi
resp, err := http.Get("https://www.example.com/File")
if err != nil {
    fmt.Println("Virhe latauksessa:", err)
}
defer resp.Body.Close()

// Luetaan ja tallennetaan tiedostoon
out, err := os.Create("example.html")
defer out.Close()
body, err := ioutil.ReadAll(resp.Body)
out.Write(body)
```

## Syvällinen sukellus

Kuten huomaamme yllä, Go-kielessä web-sivun lataaminen on hyvin yksinkertaista. Voimme myös käsitellä muita HTTP-pyyntöön liittyviä asioita, kuten asettaa otsikot tai ladata tiedostoja. Go tarjoaa myös mahdollisuuden käyttää `goroutines` ja `channels` tehokkaampaan ja rinnakkaiseen web-sivun lataukseen.

## Katso myös

- [Go-net/http paketin dokumentaatio (englanniksi)](https://golang.org/pkg/net/http/)
- [Go-net/http tutorial (englanniksi)](https://golang.org/doc/tutorial/web-service-gin)
- [Go-std/io paketin dokumentaatio (englanniksi)](https://golang.org/pkg/std/io/)