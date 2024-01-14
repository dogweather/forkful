---
title:    "Clojure: Puomien lukeminen komentoriviltä"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Miksi

Useimmissa ohjelmointiprojekteissa on tarpeen lukea komentoriviparametreja, jotta ohjelma voi toimia eri olosuhteissa. Tässä blogipostissa tutustumme siihen, miten tämä voidaan tehdä Clojure-kielellä ja miksi se on tärkeää.

## Kuinka tehdä

Komentoriviparametrien lukeminen Clojurella on helppoa käyttämällä `(command-line-args)`-funktiota. Tämä palauttaa listan annetuista komentoriviparametreista. Katso esimerkki koodilohkosta alla:

```Clojure
(def args (command-line-args))
(println "Annetut komentoriviparametrit:")
(doseq [arg args]
  (println arg))
```

Kun suoritat tämän Clojure-koodin komentoriviltä antaen esimerkiksi parametrit `hello` ja `world`, saat tulosteen:

```
Annetut komentoriviparametrit:
hello
world
```

## Syvemmälle

Komentoriviparametrien lukemisessa on tärkeää huomioida, että ne tulee antaa oikeassa järjestyksessä ja että ne tulee käsitellä oikein. Usein ohjelmissa tarvitaan myös tarkistuksia ja virheenkäsittelyä komentoriviparametreille, esimerkiksi tarkistaa, että annetut parametrit ovat oikeassa muodossa.

Clojurella on myös mahdollista käyttää kirjastoa `(tools.cli)`, joka tarjoaa hyödyllisiä toimintoja komentoriviparametrien lukemiseen ja käsittelyyn.

## Katso myös

- [https://clojuredocs.org/clojure.core/command-line-args](https://clojuredocs.org/clojure.core/command-line-args)
- [https://github.com/clojure/tools.cli](https://github.com/clojure/tools.cli)