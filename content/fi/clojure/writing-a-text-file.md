---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "Clojure: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

"Mikä & Miksi?":
Tekstitiedoston kirjoittaminen on periaatteessa vain tekstin tallentamista tietokoneesi muistiin. Ohjelmoijat käyttävät tekstitiedostoja tallentaakseen tietoa tai koodia, jota he haluavat käyttää myöhemmin.

"Kuinka tehdä?":
```Clojure
;; Luo uusi tekstitiedosto nimeltä "tietoa.txt"
(with-open [f (io/writer "tietoa.txt")]
  (.write f "Tervetuloa Ohjelmointi 101 kurssille!"))

;; Avaa ja lue tekstitiedosto
(with-open [f (io/reader "tietoa.txt")]
  (println (.readLine f)))
=> Tulostaa: "Tervetuloa Ohjelmointi 101 kurssille!"
```

"Syväsukellus":
Kirjoittaminen tekstitiedostoihin on ollut osa ohjelmointia jo pitkään. Tekstitiedostoja käytetään usein tallentamaan tietokantakyselyjä tai tulostamaan tietoa käyttäjälle. On myös muita tapoja tallentaa tietoa, kuten tietokantoihin tai verkkopalveluihin, mutta tekstitiedostot ovat yksinkertaisin ja helpoin tapa tallentaa ja lukea tietoa ohjelmassa.

"Katso myös":
Lisätietoa tekstitiedostojen lukemisesta ja kirjoittamisesta Clojurella löytyy esim. täältä: https://clojuredocs.org/clojure.java.io/writer. Voit myös kokeilla erilaisia tapoja kirjoittaa ja lukea tekstitiedostoja ja tutkia niiden toimintaa Clojure REPL:ssä.