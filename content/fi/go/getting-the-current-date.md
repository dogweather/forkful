---
title:                "Päivämäärän hankkiminen"
html_title:           "Go: Päivämäärän hankkiminen"
simple_title:         "Päivämäärän hankkiminen"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Saamme usein tarpeen tietää tämänhetkisen päivämäärän ohjelmissamme. Tämä voi olla hyödyllistä esimerkiksi aikaleimojen luomisessa tai tiedon tallentamisessa. Siksi ohjelmoijat usein lisäävät tämän toiminnallisuuden ohjelmiinsa.

## Kuinka tehdä:
Käytä Go-kielellä integroitua time-pakettia saadaksesi tietää tämänhetkisen päivämäärän. Käytössä on kaksi päivämäärän hankkimiseen liittyvää funktiota: Now() ja Local(). Esimerkiksi:

```Go
tänäänPvm := time.Now()
paikallinenPvm := time.Local()
```

Tulosteena saat tänäänPvm-arvoksi tämän päivän päivämäärän ja ajan ja paikallinenPvm-arvoksi nykyisen paikallisen päivämäärän ja ajan.

## Syventävä tarkastelu:
Time-paketti on osa Go-kielen standardikirjastoa ja se sisältää useita hyödyllisiä toimintoja ajan käsittelyyn. Tämän lisäksi on olemassa myös muita paketteja, kuten GoTime tai TimeUtil, jotka tarjoavat samankaltaisia toimintoja tämänhetkisen päivämäärän hankkimiseen. Kannattaa tutustua erilaisiin vaihtoehtoihin ja valita sopivin tarpeisiisi.

## Tärkeää huomioitavaa:
Time-paketista löytyy myös Parse()-funktio, jolla voit muuttaa merkkijonon päivämääräksi ja aikaleimaksi. Lisäksi paketissa on myös muita hyödyllisiä toimintoja ajan käsittelyyn, kuten aikaerojen laskeminen ja aikavyöhykkeiden muuttaminen.

## Katso myös:
- [Go:n ajan käsittely -opas](https://golang.org/pkg/time/)
- [GoTime-paketti](https://github.com/go-time)
- [TimeUtil-paketti](https://github.com/TimeUtil)