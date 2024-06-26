---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:17.315610-07:00
description: "Kuinka tehd\xE4: Clojuren saumaton yhteentoimivuus Javan kanssa antaa\
  \ sinun hy\xF6dynt\xE4\xE4 Java Date-Time API:a suoraan. N\xE4in voit saada nykyisen\
  \ p\xE4iv\xE4m\xE4\xE4r\xE4n."
lastmod: '2024-03-13T22:44:56.194639-06:00'
model: gpt-4-0125-preview
summary: "Clojuren saumaton yhteentoimivuus Javan kanssa antaa sinun hy\xF6dynt\xE4\
  \xE4 Java Date-Time API:a suoraan."
title: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hankkiminen"
weight: 29
---

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
