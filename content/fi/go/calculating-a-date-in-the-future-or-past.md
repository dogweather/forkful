---
title:                "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
html_title:           "Go: Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Lasketaan päivämäärä tulevaisuudessa tai menneisyydessä Go-kielellä

## Mikä & Miksi?

Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä tarkoittaa tulevan tai menneen päivämäärän määrittämistä nykyhetkestä. Ohjelmoijat tekevät tämän ajanhallintaan sovelluksissaan, esimerkiksi tehtävien ajoittamiseen tai vanhentuneiden tietojen siivoamiseen.

## Näin tehdään:

Aloitetaan lataamalla `time` paketti, joka sisältää kaikki tarvittavat työkalut.

```Go
package main
import "time"
```

Lasketaan päivämäärä 3 päivän päästä:

```Go
func main() {
        t := time.Now()
        t = t.AddDate(0, 0, 3)
        println("Päivämäärä 3 päivän päästä on :", t.Format("2006-01-02"))
}
```
Koodin ajamisen jälkeen tulostuu päivämäärä 3 päivän päästä. 

Lasketaan päivämäärä 2 viikkoa sitten:

```Go
func main() {
      t := time.Now()
      t = t.AddDate(0, 0, -14)
      println("Päivämäärä 2 viikkoa sitten oli :", t.Format("2006-01-02"))
}
```
Koodin ajamisen jälkeen tulostuu päivämäärä 2 viikkoa sitten.

## Syvällisemmin:

Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä on ollut ohjelmoinnin perusosa jo sen alkuvuosista lähtien. Järjestelmät kuten Unix timestamp, joka laskee sekunteja 1. tammikuuta 1970 alkaen, ovat standardina monissa järjestelmissä.

Suosittu vaihtoehto `time` paketille on `date` paketti, joka tarjoaa enemmän joustavuutta ja säädettävyyttä päivämäärien käsittelyyn.

Go:n `AddDate` funktio laskee uuden päivämäärän lisäämällä tai poistamalla vuosia, kuukausia tai päiviä nykyisestä päivämäärästä. Jos se ylittää kuukauden päivät tai putoaa negatiiviseksi, se säätää kuukauden.

## Katso myös:

- [`time` Paketin Dokumentaatio](https://golang.org/pkg/time/)
- [Unix Timestampin Historia](https://en.wikipedia.org/wiki/Unix_time)
- [`date` Paketti](https://github.com/rickb777/date)