---
title:                "Go: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi
Go-ohjelmointi on yhä suositumpi ohjelmointikieli, jota käytetään monissa sovelluskehitysprojekteissa. Yksi hyödyllisistä taidoista, joita voi oppia Go-ohjelmoinnissa, on päivämäärän muuntaminen merkkijonoksi. Tämä taito voi olla hyödyllinen esimerkiksi luetun päivämäärän tallentamisessa tietokantaan tai näyttämiseen käyttäjän näkymässä.

## Kuinka tehdä se

Päivämäärän muuntaminen merkkijonoksi onnistuu helposti Go-kielellä. Ensiksi täytyy tuoda Go:n aikakirjasto käyttöön. Sitten voit käyttää aikakirjaston `Format`-funktiota, joka hyväksyy kaksi parametria: päivämäärän formaatti ja itse päivämäärän.
```
import "time"

func main() {
  date := time.Now()
  dateString := date.Format("02.01.2006")
  fmt.Println(dateString)
}
```
Yllä olevassa koodissa ensin haetaan nykyinen päivämäärä `time.Now()` funktion avulla ja muunnetaan se merkkijonoksi `date.Format` funktion avulla. Päivämäärän formaatissa käytetään Esimerkissä "02" merkitsee päivän numeroa, "01" kuukauden ja "2006" vuoden lukua. Voit muuttaa näitä numeroita halutessasi saadaksesi erilaisia muotoja. Tulosteena pitäisi olla nykyinen päivämäärä muodossa "27.06.2021".

## Syvemmälle

Go:n aikakirjasto tarjoaa paljon erilaisia vaihtoehtoja päivämäärän muotoiluun. Esimerkiksi, jos haluat näyttää myös ajan ja kellonajan, niin voit käyttää `Format`-funktion sijaan `DateTime`-funktiota.

```
dateWithTime := date.Format("02.01.2006 15:04:05")
```

Voit myös käyttää muita päivämäärän muotoilujen erikoisia, kuten "Mon Jan 2 15:04:05 MST 2006". Voit lukea lisää vaihtoehdoista Go:n virallisesta dokumentaatiosta.

## Katso myös

- [Go:n virallinen dokumentaatio (englanniksi)](https://golang.org/pkg/time/#Time.Format)
- [Lyhyt esimerkki päivämäärän muuntamisesta merkkijonoksi (englanniksi)](https://gobyexample.com/time-formatting-parsing)