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

## Mitä ja miksi?
Päivämäärän muuttaminen merkkijonoksi on yleinen ohjelmointitehtävä, jossa päivämäärän arvo muuttuu helposti luettavaan muotoon esimerkiksi in ennen tallentamista tietokantaan tai tiedostoon. Ohjelmoijat käyttävät tätä tehtävää parantaakseen ohjelman käytettävyyttä ja muotoiluja.

## Miten tehdä:
```Clojure
(require '[clojure.java-time :as t])
(t/format (t/date-time 2021 1 1) "dd/MM/yyyy") ;;=> "01/01/2021"
```

Voit käyttää `clojure.java-time`-kirjastoa päivämäärän muuntamiseen merkkijonoksi. Käytämme `format`-funktiota, joka ottaa parametreiksi päivämäärän ja halutun muotoilun. Voit myös muuttaa aikavyöhykkeen antamalla lisäparametrinä halutun aikavyöhykkeen.

## Syväsukellus:
On tärkeää huomata, että päivämäärämuotoilu on ollut haaste ohjelmoinnissa aina ennen kehittäjäyhteisöjen luomista. Nykyään on olemassa useita vaihtoehtoja päivämäärän muuttamiseksi merkkijonoksi, mutta `clojure.java-time` on yksi suosituimmista vaihtoehdoista Clojure-ohjelmoijille. Tämä kirjasto käyttää Javassa olevia luokkia päivämäärän käsittelyyn, joten se tarjoaa luotettavan ja tehokkaan tavan tehdä päivämäärään liittyviä toimintoja.

## Katso myös:
- [Clojure.java-time GitHub-sivu](https://github.com/dm3/clojure-java-time)
- [Virallinen Java-päivämääräkirjasto](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)