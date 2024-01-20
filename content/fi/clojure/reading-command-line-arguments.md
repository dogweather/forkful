---
title:                "Komentorivin argumenttien lukeminen"
html_title:           "Elm: Komentorivin argumenttien lukeminen"
simple_title:         "Komentorivin argumenttien lukeminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mikä ja Miksi?

Komentoriviparametrien lukeminen on prosessi, jossa ohjelmisto ottaa käyttäjän syöttämät argumentit tai parametrit suoraan käytettäviksi ohjelmassa. Tätä tehdään yleensä, koska se antaa joustavuutta ohjelman käyttöön, mahdollistaen sen, että samaa koodia voidaan käyttää eri tarkoituksiin ilman, että sitä tarvitsee uudelleenkirjoittaa.

## Näin se tehdään:

Voit lukea Clojuressa komentoriviparametreja käyttämällä 'clojure.core/*command-line-args* -muuttujaa. Katsotaan esimerkkiä:

```Clojure
(defn -main
  [& args]
  (println "Tervetuloa! Tässä ovat syöttämäsi argumentit:" args))

;; Käynnistä ohjelma komentoriviltä argumenteilla 'Hei' ja 'Maailma'
;; Tulostaa "Tervetuloa! Tässä ovat syöttämäsi argumentit: (Hei Maailma)"
```

## Syvällinen sukellus:

Historiallisesti komentoriviparametrin lukeminen on ollut oleellinen osa komentoriviohjelmistojen suunnittelua Unix-järjestelmistä lähtien. Vaikka nykyaikaisilla ohjelmointikielillä on useita vaihtoehtoja parametrien käsittelyyn, Clojure vaalii yksinkertaisuutta ja minimalismia, ja midnightship/java käytetään. Huomaathan, että '*'clojure.core/*command-line-args**'- muuttuja on saatavilla vain ohjelmasi käynnistämisen aikana.

## Katso myös:

- Hyvä perehdytys komentoriviparametrien lukemisen perusteisiin on saatavilla [Clojure Doc](https://clojure.org/guides/getting_started#_command_line_arguments) sivuilla.
- Lisätietoja komentoriviparametrien käsittelystä Midnightship/javan kanssa löydät seuraavasta [linkistä](https://github.com/bhauman/lein-figwheel/wiki/Node.js-development-with-figwheel).