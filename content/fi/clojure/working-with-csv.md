---
title:                "Törmätä csv-tiedostoihin"
html_title:           "Clojure: Törmätä csv-tiedostoihin"
simple_title:         "Törmätä csv-tiedostoihin"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

CSV (Comma-Separated Values) tiedostot ovat tekstimuotoisia taulukoita, joissa tunnistemerkki erottelee eri sarakkeiden arvot. Ohjelmoijat käyttävät CSV-tiedostoja usein tietojen tallentamiseen ja jakamiseen, sillä ne ovat helppolukuisia ja monien sovellusten yhteensopivia.

## Miten:

Voit lukea CSV-tiedostoja Clojurella "clojure-csv" kirjastolla. Kirjaston asentamiseen voit käyttää esimerkiksi "Leiningen" työkalua. Tämän jälkeen voit käyttää seuraavaa esimerkkikoodia CSV-tiedoston lukemiseen ja sarakkeiden tietojen tulostamiseen:

```clojure
(use 'clojure-csv.core)
(with-open [file (reader "tiedostonimi.csv")]
  (doseq [line (parse-csv file)]
    (prn line)))
```

Tämä koodi lukee tiedoston "tiedostonimi.csv" ja tulostaa jokaisen rivin erikseen.

## Syväsukellus:

CSV-tiedostot kehitettiin alunperin tiedonsiirtoa varten tietokoneiden ja ohjelmien välillä. Nykyään CSV-tiedostoja käytetään usein myös esimerkiksi taulukkolaskentaohjelmissa ja verkko-sovelluksissa. Vaihtoehtoisia tapoja työskennellä CSV-tiedostojen kanssa ovat esimerkiksi "Clojure.data.zip" kirjasto ja "clojure-csv" kirjaston uudempi versio "clojure-csv2".

## Katso myös:

[clojure-csv - GitHub](https://github.com/davidsantiago/clojure-csv)

[Clojure.data.zip - GitHub](https://github.com/clojure/data.zip)