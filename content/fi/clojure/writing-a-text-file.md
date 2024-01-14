---
title:    "Clojure: Tekstitiedoston kirjoittaminen"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Miksi

Teksti-tiedoston kirjoittaminen on olennainen osa ohjelmointia. Se mahdollistaa ohjelmoijille tallentaa ja jakaa tietoa koodin toiminnasta ja tarkoituksesta.

## Miten

Ohjelmointikyselyn aloittaminen on helppoa Clojurella. Aloita avaamalla tekstieditori ja kirjoittamalla seuraava koodinpätkä:

```Clojure
(defn teksti-tiedosto [teksti]
  (spit "tiedosto.txt" teksti))
```

Tämä luo funktion nimeltä "teksti-tiedosto", joka ottaa parametrinaan tekstin ja tallentaa sen "tiedosto.txt" -tiedostoon. Voit kutsua tätä funktiota seuraavasti:

```Clojure
(teksti-tiedosto "Hei, tämä on teksti-tiedosto.")
```

Tämän pitäisi luoda tiedosto nimeltä "tiedosto.txt" ja tallentaa siihen annettu teksti. Voit nyt avata tiedoston ja tarkistaa, että se tallennettiin oikein.

## Syvällisempi sukellus

Clojurella on monia eri tapoja kirjoittaa teksti-tiedostoja, mutta yksi yleisimmin käytetyistä on "spit" -funktio, jonka käytimme esimerkissä. Tämä funktio ottaa ensimmäisenä parametrinaan tiedoston nimen ja toisena parametrinaan tallennettavan tekstin.

Voit myös käyttää "slurp" -funktiota lukeaksesi tekstitiedoston sisällön ja "with-open" -lauseketta avataksesi ja sulkeaksesi tiedoston automaattisesti.

```Clojure
(defn lue-tiedosto [tiedosto]
  (slurp tiedosto))

(with-open [tiedosto (reader "tiedosto.txt")]
  (println (lue-tiedosto tiedosto)))
```

## Katso myös
- [Clojure dokumentaatio](https://clojure.org/index)
- [Clojure rekisteröinti](https://clojuredocs.org/)
- [Clojure-kurssit](https://www.clojuristuksenkeltainenkirja.fi/kurssit)