---
title:    "Clojure: Puuhan poistaminen kuvioon sopivilta merkeiltä"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit poistaa merkkejä jotka vastaavat tiettyä kaavaa? Ehkä sinulla on iso tekstiaineisto ja haluat selventää sitä poistamalla tarpeettomat merkit. Tämä voi myös auttaa sinua löytämään tietoa kokonaisuudessaan tiettyä kaavaa vastaavista merkeistä.

## Kuinka tehdä

```Clojure
(defn poista-merkit [teksti kaava]
  (clojure.string/replace
    teksti
    kaava
    ""))
    
(poista-merkit "Tervetuloa maailmaan!" #"[aeiou]")
=> "Trvltn mlmn!"

(poista-merkit "Tämä on vain esimerkki." #"\W")
=> "Tämäonvainesimerkki"
```

## Syventävä sukellus

Funktiota "poista-merkit" käytetään Clojuren "clojure.string/replace" -metodin avulla. "replace" korvaa kaikki annetun kaavan mukaiset merkit tyhjällä merkillä, jolloin ne poistetaan kokonaan. Kaava on kirjoitettu säännöllisten lausekkeiden muodossa, joka mahdollistaa tarvittavien merkkien tarkemman määrittämisen.

## Katso myös

- [Säännölliset lausekkeet](https://clojure.org/reference/regular_expressions)
- [clojure.string/replace dokumentaatio](https://clojuredocs.org/clojure.string/replace)