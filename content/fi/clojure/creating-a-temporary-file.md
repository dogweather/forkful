---
title:    "Clojure: Väliaikaisen tiedoston luominen"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi luoda väliaikaisia tiedostoja?

Väliaikaisten tiedostojen luominen on tärkeää monissa ohjelmointitehtävissä, kun halutaan tallentaa väliaikainen tieto, jota ei tarvita ohjelman suorituksen loppuvaiheessa. Tällaisia tehtäviä voivat olla esimerkiksi tietokannan varmuuskopiointi tai väliaikaisen tiedon tallentaminen ennen sen käsittelyä.

## Kuinka tehdä se

```Clojure
; Luo väliaikainen tiedosto
(with-open [temp-file (java.io.File/createTempFile "temporary" "txt")]
  ; Kirjoita tiedostoon tekstit
  (with-open [writer (clojure.java.io/writer temp-file)]
    (.write writer "Tämä on väliaikainen tiedosto.")))
```

Tulostus:

```
temporary1979337958178079866.txt
Tämä on väliaikainen tiedosto.
```

## Syvällisempää tietoa väliaikaisten tiedostojen luomisesta

Väliaikaisten tiedostojen luominen Clojurella on yksinkertaista käyttämällä Java-luokkia ja Clojuren `with-open` -makroa. Tiedostolle annetaan nimi ja tiedoston tyyppi, jonka jälkeen sen sisään voi kirjoittaa halutun sisällön. Tärkeää on huolehtia, että tiedosto suljetaan `with-open` -makron sisällä, jotta se poistetaan automaattisesti ohjelman suorituksen loputtua.

## Katso myös

- [Creating temporary files in Clojure](https://gist.github.com/ghoseb/cff71989e7d3ec01ea8f)
- [Clojure's with-open explained](https://clojureverse.org/t/clojures-with-open-explained/4326)
- [Using temporary files in Clojure for efficient resource management](https://gleamynode.net/articles/2215/)