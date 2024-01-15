---
title:                "Päivämäärän muuntaminen merkkijonoksi"
html_title:           "Clojure: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmoinnissa tarvitsee muuttaa päivämäärä merkkijonoksi, esimerkiksi tulostettaessa päivämäärää käyttäjälle näkyvässä muodossa. Clojurella tämä on helppoa ja nopeaa tehdä.

## Miten tehdä se

Merkkijonoksi muuttaminen vaatii ensin päivämäärän konvertoinnin Date-tyypiksi. Tämän jälkeen voi käyttää `format`-funktiota muuttaakseen päivämäärän halutussa muodossa olevaksi merkkijonoksi.

```
;; Konvertoi päivämäärän merkkijonosta Date-tyyppiin
(def date (java.util.Date. "2021-02-01"))

;; Muuttaa päivämäärän formaatissa "dd/MM/yyyy" merkkijonoksi (tulostaa "01/02/2021")
(def formatted-date (format date "dd/MM/yyyy"))
```

`format`-funktion ensimmäinen argumentti on muunnettava päivämäärä Date-tyypissä ja toinen argumentti on haluttu muoto merkkijonona. Muotoilussa voi käyttää erilaisia merkkejä, kuten esimerkiksi `dd` päivän numerona tai `MMM` lyhenteenä kuukaudelle.

## Syvempi sukellus

Clojuren `format`-funktio hyödyntää Java Platform, Standard Edition (java.time) -kirjastoa, joten sen avulla voi myös käyttää Java-kirjaston muita toimintoja. Lisäksi `format` tukee myös paikallisia päivämäärämuotoiluja, eli päivämäärän voi esittää eri kielillä ja kulttuureissa hyödyntäen Clojuren `Locale`-tyyppiä.

## Katso myös

- [ClojureDocs: format-funktion dokumentaatio](https://clojuredocs.org/clojure.string/format)
- [Java 8 SDK - java.time - Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Locale-tyypin dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/util/Locale.html)