---
date: 2024-01-20 17:32:47.779409-07:00
description: "Verrataan kahta p\xE4iv\xE4m\xE4\xE4r\xE4\xE4, ymm\xE4rt\xE4\xE4ksemme\
  \ kummanko ajanhetki sijoittuu ennen toista. Ohjelmoijille se on t\xE4rke\xE4\xE4\
  \ ajoitukseen, tapahtumien k\xE4sittelyyn ja\u2026"
lastmod: '2024-03-11T00:14:30.126937-06:00'
model: gpt-4-1106-preview
summary: "Verrataan kahta p\xE4iv\xE4m\xE4\xE4r\xE4\xE4, ymm\xE4rt\xE4\xE4ksemme kummanko\
  \ ajanhetki sijoittuu ennen toista. Ohjelmoijille se on t\xE4rke\xE4\xE4 ajoitukseen,\
  \ tapahtumien k\xE4sittelyyn ja\u2026"
title: "Kahden p\xE4iv\xE4m\xE4\xE4r\xE4n vertailu"
---

{{< edit_this_page >}}

## What & Why?
Verrataan kahta päivämäärää, ymmärtääksemme kummanko ajanhetki sijoittuu ennen toista. Ohjelmoijille se on tärkeää ajoitukseen, tapahtumien käsittelyyn ja logiikkaan.

## How to:
Clojuren `clj-time` kirjasto on kätevä päivämäärävertailuihin: 

```Clojure
(require '[clj-time.core :as time])
(require '[clj-time.coerce :as coerce])

(let [date1 (coerce/to-date-time "2021-01-01T12:00:00.000Z")
      date2 (coerce/to-date-time "2021-12-31T16:00:00.000Z")]
  {:before (time/before? date1 date2)
   :after  (time/after? date1 date2)
   :equal  (time/equal? date1 date2)})
```
Tuloste:
```Clojure
{:before true, :after false, :equal false}
```

## Deep Dive
Alun perin Clojure-kielessä päivämäärien käsittely oli java.util.Date-luokan varassa. `clj-time` perustuu Joda-Time-kirjastoon, joka on vanhan `java.util.Date`-luokan parempi versio. Jos `clj-time` ei ole käytössä, voi käyttää myös `java.time`-kirjastoa:

`java.time`-esimerkki:

```Clojure
(import java.time.ZonedDateTime)

(defn compare-dates [date1-str date2-str]
  (let [date1 (ZonedDateTime/parse date1-str)
        date2 (ZonedDateTime/parse date2-str)]
    (cond
      (.isBefore date1 date2) "date1 is before date2"
      (.isAfter date1 date2)  "date1 is after date2"
      :else "dates are equal")))

(compare-dates "2021-01-01T12:00:00Z" "2021-12-31T16:00:00Z")
```
Tuloste:
```Clojure
"date1 is before date2"
```

Verrattuna Joda-Timeen `java.time` on osa Java 8:aa ja sitä uudempia versioita.

Päivämäärien vertailu voi tuntua suoraviivaiselta, mutta aikavyöhykkeet ja kesäaikaan siirtyminen tekevät siitä monimutkaista. On tärkeää valita oikea kirjasto, joka käsittelee nämä yksityiskohdat.

## See Also
- `clj-time` GitHub-sivusto: https://github.com/clj-time/clj-time
- Java 8 `java.time` dokumentaatio: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- Joda-Time projekti: http://www.joda.org/joda-time/

Nämä materiaalit auttavat syventämään ymmärrystä päivämäärien käsittelystä Clojuressa.
