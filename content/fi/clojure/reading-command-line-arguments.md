---
title:                "Komentoriviparametrien lukeminen"
html_title:           "Clojure: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Jos haluat hallita tietokoneesi tai ohjelmasi käynnistystä ja toimintaa komentoriviltä, tarvitset tietoa siitä, miten lukea komentorivin argumentteja. Se on hyödyllistä esimerkiksi skriptien ja ohjelmien kehittämisen ja testaamisen aikana.

## Miten

Voit lukea komentorivin argumentteja Clojure-kielellä helposti `command-line-args`-funktion avulla. Voit käyttää sitä esimerkiksi seuraavalla tavalla:

```
Vaihtoehdot komentoriviltä:

$ clojure -m ohjelma
Oletusasetuksilla käynnistyvä ohjelma

$ clojure -m ohjelma -tiedosto testi.txt
Ohjelma käynnistyy ja lukee tiedoston testi.txt
```

```Clojure
(ns ohjelma
  (:require [clojure.edn :as edn]))

(defn -main
  [& args]
  (let [options (edn/read-string (first args))]
    (println "Vaihtoehdot komentoriviltä:")
    (println options)
    (println "Oletusasetuksilla käynnistyvä ohjelma")))

;; Käyttämällä `-m`-vaihtoehtoa, voit antaa ohjelmalle parametreja
```

Tässä esimerkissä käytetään `edn`-kirjastoa lukemaan komentorivin vaihtoehdot ja tulostetaan ne konsoliin.

## Syväsukellus

Komentorivin argumenttien lukeminen tapahtuu käyttämällä Clojuren integroimaa Java-luokkaa `java.lang.System`. Tässä luokassa on `getProperty`-funktio, joka palauttaa komentorivin argumenttina annetun arvon.

Voit myös käyttää Clojuren `command-line-args`-kirjastoa, joka tekee samaa asiaa mutta käsittelee argumentit suoraan kokoelmaksi.

Voit myös käyttää `getenv`-funktiota, joka lukee ympäristömuuttujista.

## Katso myös

- [Clojure-command-line-args](https://github.com/clojure/tools.cli)
- [Java-luokka System](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/System.html)
- [Clojure stringien lukeminen komentoriviltä](https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/parse-opts)