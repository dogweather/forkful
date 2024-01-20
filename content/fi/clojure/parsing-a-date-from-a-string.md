---
title:                "Merkkijonosta päivämäärän jäsentäminen"
date:                  2024-01-20T15:35:23.466682-07:00
html_title:           "Bash: Merkkijonosta päivämäärän jäsentäminen"
simple_title:         "Merkkijonosta päivämäärän jäsentäminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? / Mikä ja Miksi?
Päivämäärän jäsentäminen merkkijonosta tarkoittaa päivämäärä-tiedon erottelemista ja muuttamista ymmärrettävään muotoon. Ohjelmoijat tekevät tätä datan validoinnin, tallennuksen ja käsittelemisen helpottamiseksi.

## How to / Kuinka tehdä:
```Clojure
(require '[java.time.format :as dtf])
(require '[java.time.LocalDate :as ld])

;; Määritellään päivämäärän formaatti ja jäsentäjä
(def date-format (dtf/DateTimeFormatter/ofPattern "dd.MM.yyyy"))
(defn parse-date [date-string]
  (ld/parse date-string date-format))

;; Esimerkki päivämäärän jäsentämisestä
(parse-date "01.05.2023")
;; => #object[java.time.LocalDate "2023-05-01"]
```

Esimerkki kertoo, kuinka luodaan päivämäärän jäsentäjä halutulla formaatilla ja jäsentää merkkijono päivämääräksi Clojuressa käyttäen java.time-kirjastoa.

## Deep Dive / Syväsukellus
Päivämäärän jäsentämisen historia ulottuu ohjelmoinnin alkuaikoihin, kun tiedon standardoitu esitysmuoto oli tarpeen. Vaihtoehtoja `java.time`-kirjastolle ovat muun muassa Joda-Time ja `clj-time`-kirjasto, mutta `java.time` on nykyisin standardi Clojuressa, sillä se on suorituskykyinen ja monipuolinen.

Clojure on suunniteltu toimimaan tehokkaasti Javan ekosysteemissä, joten hyödynnämme tässä nativiisti Javan `java.time`-kirjaston luokkia päivämäärän käsittelyyn. `java.time.LocalDate`-luokka kuvaa päivämäärää ilman kellonaikaa, joten se on ihanteellinen pelkän päivämäärän käsittelyyn.

## See Also / Katso Myös
- Official Java `java.time` documentation: [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- Clojure for the Brave and True (a book with Clojure programming basics): [https://www.braveclojure.com](https://www.braveclojure.com)
- ClojureDocs, a community-powered documentation and examples repository: [https://clojuredocs.org](https://clojuredocs.org)