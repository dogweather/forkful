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

Miten lukea komentoriviparametreja Clojuressa

## Mitä & Miksi?

Komentoriviparametrien lukeminen tarkoittaa sitä, että ohjelma pystyy ottamaan vastaan käyttäjältä komentorivillä annettuja lisäparametreja, joita voidaan käyttää ohjelman suorittamisessa. Tällä tavoin ohjelma voidaan muokata erilaisiin tarpeisiin sopivaksi. Kehittäjät käyttävät tätä toimintoa, jotta ohjelmasta saataisiin joustavampi ja monipuolisempi.

## Miten:

Clojuressa komentoriviparametrien lukeminen on helppoa ja suoraviivaista. Voit käyttää funktiota ```clojure.core/command-line``` saadaksesi komentoriviparametreista hajautetun rakenteen. Tämän jälkeen voit käyttää normaaleja Clojure-lausujeita käsitelläksesi parametreja.

Esimerkki:

```clojure
(ns komentoriviparametrit)

(let [parametrit (command-line)] ; tallennetaan parametrit muuttujaan
    (println "Tämä ohjelma ottaa vastaan seuraavat parametrit:") ; tulostetaan viesti
    (doseq [[parametri arvo] parametrit] ; käydään läpi parametrit yksi kerrallaan
        (println parametri ": " arvo))) ; tulostetaan parametri ja sen arvo
```

Kun suoritat tämän esimerkin komentoriviltä, voit antaa sille erilaisia parametreja ja näet, kuinka ohjelma tulostaa ne kaikki.

Esimerkkituloste:

```
$ java -jar komentoriviparametrit.jar --nimi John --ika 25

Tämä ohjelma ottaa vastaan seuraavat parametrit:
--nimi : John
--ika : 25
```

## Syvällinen sukellus:

Komentoriviparametrien lukemisen historia ulottuu aina ensimmäisten tietokoneiden ajoista, kun ohjelmia suoritettiin päätermillä käyttäen käsin annettuja parametreja. Nykypäivänä suurin osa ohjelmista pystyy lukemaan komentoriviparametreja, ja Clojure ei ole poikkeus.

On olemassa myös muita vaihtoehtoja kuin ```command-line```-funktio, kuten esimerkiksi kirjasto ```tools.cli```, joka tarjoaa vieläkin suuremman joustavuuden parametrien käsittelyyn.

Komentoriviparametrien lukeminen Clojuressa perustuu alustariippuvaisiin tietoihin, joten tarkista Clojure-dokumentaatiosta mitä vaihtoehtoja sinulla on koneellasi saatavilla.

## Katso myös:

- [Clojure-dokumentaatio](https://clojure.org/reference/repl_and_main#_command_line_arguments)
- [tools.cli](https://github.com/clojure/tools.cli)