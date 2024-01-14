---
title:                "Clojure: Tiedoston lukeminen"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi lukisi tiedostoa? Tämä on hyvä kysymys, ja vastaus on monimuotoinen. Tiedostojen lukeminen on olennainen osa ohjelmointia ja se voi auttaa meitä tekemään monia erilaisia ​​tehtäviä. Olipa kyseessä sitten tietokannan tietojen lajittelu, tekstin analysointi tai vain yksinkertainen tiedon lukeminen, tiedostojen lukeminen on tärkeä taito jokaiselle Clojure-ohjelmoijalle.

## Miten

Joten miten sitten lukea tiedosto Clojurella? Pidätkö varmasti seuraavasta yksinkertaisesta esimerkistä:

```Clojure
(def file (slurp "tiedostonimi.txt"))
(println file)
```

Tässä koodissa käytämme slurp-funktiota, joka lukee tiedoston merkkijonona ja tallentaa sen "file" -muuttujaan. Sitten voimme tulostaa "file" -muuttujan sisällön käyttämällä println-funktiota. 

Esimerkiksi jos tiedostossa olisi seuraava teksti:

```
Tämä on esimerkki tiedostosta
jota haluamme lukea
```

Tulostus olisi:

```
Tämä on esimerkki tiedostosta
jota haluamme lukea
```

## Syventävä sukellus

Jotta ymmärtäisimme paremmin, kuinka tiedostonlukeminen toimii Clojurella, on hyödyllistä tietää muutama asia. Ensinnäkin, slurp-funktio lukee tiedoston sisällön kokonaisuudessaan ja tallentaa sen merkkijonona muuttujaan. Tämä tarkoittaa, että tiedoston luku voi olla hidasta, jos tiedosto on suuri. 

Toiseksi, slurp-funktio olettaa, että tiedoston koodaus on UTF-8. Jos käytät erilaista koodausta, voit määrittää sen slurpin valinnaiseen toiseen parametriin.

Lisäksi voi olla hyödyllistä käsitellä tiedostoa rivi kerrallaan käyttämällä esimerkiksi 'with-open'-rakenne, joka sulkee tiedoston automaattisesti kun lukeminen on valmis. 

## Katso myös

- [Clojure-kirjaston tiedostojen käsittely](https://clojure.org/reference/io) 
- [Slurp-dokumentaatio](https://clojuredocs.org/clojure.core/slurp) 
- [Clojure-koodin lähde](https://github.com/clojure/clojure/blob/master/src/clj/clojure/core.clj#L5977)