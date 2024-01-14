---
title:                "Clojure: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi muuttaisit päivämäärän merkkijonoksi ohjelmointikielessä Clojure?

## Miten

```Clojure
(let [today (java.util.Date.)
      string-date (str today)]
  (println string-date))
```

Tässä yksinkertaisessa esimerkissä käytämme Clojuren `str`-funktiota muuttaaksemme Java-luokan `java.util.Date` instanssin merkkijonoksi. Lopputuloksena tulostamme nykyisen päivämäärän ja ajan.

```
Tue Mar 30 16:54:53 EEST 2021
```

## Syvällinen sukellus

Päivämäärän muuttaminen merkkijonoksi voi olla hyödyllistä esimerkiksi tietokannassa tallennettujen päivämäärien esittämiseen käyttäjälle. Voit myös muuttaa päivämäärän haluamaasi muotoon Clojuren `format`-funktion avulla.

```Clojure
(require '[clojure.java-time :as time])

(let [today (time/today)
      formatted-date (time/format today "dd.MM.yyyy")]
  (println formatted-date))
```

Tässä käytämme Clojuren `clojure.java-time` kirjastoa helpottamaan päivämäärän formatointia. Lopputuloksena saamme päivämäärän merkkijonossa muodossa "30.03.2021".

## Katso myös

- [Clojuren virallinen dokumentaatio Java-integraatiosta](https://clojure.org/reference/java_interop)
- [Clojuren `clojure.java-time` kirjaston dokumentaatio](https://github.com/clj-time/clj-time)