---
title:    "Clojure: Säännöllisten lausekkeiden käyttö"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita Clojure-ohjelmoinnissa?

Säännölliset lausekkeet ovat tehokas työkalu tiedon etsimiseen ja muokkaamiseen tekstipohjaisissa tiedostoissa. Niiden avulla voit etsiä tiettyjä merkkijonoja, suorittaa monimutkaisia muokkauksia ja jopa validoida käyttäjän antamia syötteitä. Clojuren sisäänrakennetut säännölliset lausekkeet tekevät tästä tehtävästä erittäin helpon ja suoraviivaisen.

## Miten käytät säännöllisiä lausekkeita Clojure-ohjelmoinnissa?

Ensimmäiseksi, sinun tulee tuoda säännölliset lausekkeet Clojureen käyttämällä ```(:require [clojure.string :as str])```. Tämän jälkeen voit käyttää ```re-find``` -funktiota etsimään merkkijonoja tietystä tekstipohjaisesta muuttujasta.

Esimerkiksi, jos haluat etsiä kaikki puhelinnumeroita, jotka ovat muodossa "xxx-xxx-xxxx", voit käyttää seuraavaa koodia:

```
(def phone-numbers "123-456-7890 555-123-4567 333-999-0000")

(str/re-find #"(\d{3}-\d{3}-\d{4})" phone-numbers)
```

Tämä palauttaisi listan löydetyistä puhelinnumeroista: ```["123-456-7890", "555-123-4567", "333-999-0000"]```.

## Syvempää tietoa säännöllisten lausekkeiden käytöstä Clojure-ohjelmoinnissa

Clojuren säännölliset lausekkeet perustuvat Java-kielen säännöllisiin lausekkeisiin, joten niiden syntaksi on hyvin samanlainen. Voit esimerkiksi käyttää erilaisia säännöllisten lausekkeiden ilmaisuja kuten tähtimerkkiä ```*``` tai kysymysmerkkiä ```?``` löytääksesi tiettyjä merkkijonoja.

On myös mahdollista käyttää säännöllisten lausekkeiden muuttujia, kuten osoittimia ja rajausmerkkejä, tarkempien hakujen tekemiseen. Voit kokeilla erilaisia säännöllisiä lausekkeita ja löytää parhaiten sopivan niille tarkoitetun ongelman ratkaisemiseksi.

## Katso myös

- [Clojure-ohjelmoinnin perusteet](https://clojure.org/guides/getting_started)
- [Java säännölliset lausekkeet](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)