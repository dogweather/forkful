---
title:                "Clojure: Muuntaminen päivämääräksi merkkijonoksi"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Miksi: Miksi muuttaa päivämäärä merkkijonoksi?

Merkkijonojen muuntaminen päivämääriksi on yksi tärkeä osa ohjelmointia. Se auttaa meitä ymmärtämään, käsittämään ja työskentelemään päivämäärien kanssa helpommin. Clojure tarjoaa helpon tavan muuntaa päivämäärät merkkijonoiksi, joten oppiminen on kannattavaa ja hyödyllistä.

# Miten tehdä: Esimerkkejä koodista ja tulostuksesta

```Clojure
(doto (java.util.Date.)
  (.toLocaleString)) ; tulostaa nykyisen päivämäärän ja ajan
; "31.12.2021 klo 12:00:00"

(str (java.time.LocalDate/now)) ; muuntaa tämän päivän päivämäärän merkkijonoksi
; "2021-12-31"

(str (java.time.LocalDate/parse "2021-06-14")) ; muuntaa annetun päivämäärän merkkijonoksi
; "2021-06-14"
```

## Syvällinen sukellus

Clojuressa päivämäärät tallennetaan java.util.Date- ja java.time.LocalDate-olioina. Jotta päivämäärät voidaan muuntaa merkkijonoiksi, on ensin luotava jokin näistä olioista. Sitten käytetään str-funktiota muuttamaan päivämäärä merkkijonoksi. Tämä on yksinkertainen ja tehokas tapa käsitellä päivämääriä Clojurella.

# Katso myös

- [Clojure dokumentaatio päivämäärämuunnoksista](https://clojuredocs.org/clojure.java-time/local-date-format)
- [Clojure Cheat Sheet: Päivämäärät ja ajat](https://clojure.org/api/cheatsheet#datestimes)
- [Java 8 LocalDate- ja LocalTime-ohjeet](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)