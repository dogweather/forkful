---
title:                "Nykyisen päivämäärän hankkiminen"
aliases:
- fi/clojure/getting-the-current-date.md
date:                  2024-02-03T19:09:17.315610-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nykyisen päivämäärän hankkiminen"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
Nykyisen päivämäärän saaminen ohjelmoinnissa on ratkaisevan tärkeää monista syistä, mukaan lukien lokitiedot, tapahtumien aikaleimat ja tehtävien ajoittaminen. Clojuressa, Lisp-murteessa JVM:llä, tämä tehtävä hyödyntää Javan välisten toimintojen mahdollisuuksia, mahdollistaen suoraviivaisen pääsyn rikkaaseen Java Date-Time API:in.

## Kuinka tehdä:

### Käyttäen Javan välistä yhteentoimivuutta
Clojuren saumaton yhteentoimivuus Javan kanssa antaa sinun hyödyntää Java Date-Time API:a suoraan. Näin voit saada nykyisen päivämäärän:

```clojure
(import java.time.LocalDate)

(defn get-current-date []
  (str (LocalDate/now)))

;; Esimerkkituloste
(get-current-date) ; "2023-04-15"
```

### Käyttäen clj-time-kirjastoa
Idiomatisemman Clojure-ratkaisun saamiseksi saatat valita `clj-time`-kirjaston, joka on kääre Joda-Timeen, vaikkakin useimmille uusille projekteille sisäänrakennettua Java 8 Date-Time API:a suositellaan. Kuitenkin, jos preferoit tai tarvitset `clj-time`:

Lisää ensin `clj-time` projektisi riippuvuuksiin. `project.clj`-tiedostossasi, sisällytä:

```clojure
[clj-time "0.15.2"]
```

Käytä sitten sitä saadaksesi nykyisen päivämäärän:

```clojure
(require '[clj-time.core :as time])

(defn get-current-date-clj-time []
  (str (time/now)))

;; Esimerkkituloste
(get-current-date-clj-time) ; "2023-04-15T12:34:56.789Z"
```

Molemmat menetelmät tarjoavat nopeat, tehokkaat tavat saada nykyinen päivämäärä Clojuressa, hyödyntäen alla olevan Javan alustan voimaa tai Clojure-spesifin kirjaston mukavuutta.
