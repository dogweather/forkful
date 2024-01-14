---
title:    "Clojure: Päivämäärän muuttaminen merkkijonoksi"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointiprojekteissa, joissa käsitellään päivämääriä, on tarve muuntaa päivämäärä merkkijonoksi joko esitysmuodon tai tietokantojen yhteensopivuuden vuoksi.

## Näin teet sen

Päivämäärän muuntaminen merkkijonoksi on helppoa Clojurella. Voit käyttää siihen `str`-funktiota yhdistettynä `format`-funktioon. Esimerkiksi:

```Clojure
(str (format "%04d-%02d-%02d" 2021 04 01))
```
Tämä tuottaa merkkijonon "2021-04-01". Voit myös käyttää `clj-time`-kirjaston `at`-funktiota ja `ISO_DATE_FORMAT`-vakiotyyppiä lisäämään päivämäärään ajan. Esimerkiksi:

```Clojure
(require '[clj-time.format :as fmt]
         '[clj-time.core :as t])
(t/at (t/date-time 2021) (fmt/ISO_DATE_FORMAT))
```
Tämä tuottaa merkkijonon "2021-01-01T00:00:00.000Z", jossa on mukana myös aika.

## Syventävä syventymiskurssi

Päivämäärän muuntaminen merkkijonoksi voi tuntua yksinkertaiselta, mutta käytännössä siihen voi liittyä tarkkoja sääntöjä ja työkaluja. Esimerkiksi tietokannoissa päivämääriä muutetaan usein valittuun aikavyöhykkeeseen ennen niiden tallentamista. Lisäksi päivämäärät voivat olla eri formaateissa eri maissa ja kulttuureissa. Clojurella on käytettävissä useita eri kirjastoja ja moduuleja, jotka helpottavat päivämäärän muuntamista haluttuun muotoon ja tarjoavat apua aikavyöhykkeen ja paikallisasetusten mukauttamisessa.

## Katso myös

- [Clj-time library](https://github.com/clj-time/clj-time)
- [Clojure date formatting knowledgebase article](https://clojure.org/guides/date_formatting)
- [JVM time and date documentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)