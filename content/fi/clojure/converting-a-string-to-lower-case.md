---
title:    "Clojure: Merkkijonon muuntaminen pieniksi kirjaimiksi"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi:

Yksi yleinen ohjelmointitehtävä on muuntaa merkkijono pienaakkosiksi. Tämä on hyödyllistä esimerkiksi tietokantojen kanssa työskentelyssä, jossa vertailu tapahtuu usein pienaakkosittain.

## Miten:

```Clojure
(def s "Tämä on TESTIMERKKIJONO")

(println (.toLowerCase s))

```
Tulostus: "tämä on testimerkkijono"

## Syvempi sukellus:

Merkkijonon muuntaminen pienaakkosiksi tapahtuu käyttämällä Clojuren '.toLowerCase' toimintoa, joka käyttää Java-kirjaston vastaavaa metodia. Tämä toiminto ottaa merkkijonon ja palauttaa uuden kopion, jossa kaikki merkit ovat pienaakkosina.

On tärkeää huomata, että tämä toiminto ei muuta alkuperäistä merkkijonoa, vaan palauttaa aina uuden kopion. Tämä voi olla hyödyllistä, jos haluat säilyttää alkuperäisen merkkijonon muuttumattomana.

## Katso myös:

- Clojure '.toUpperCase' toiminto: https://clojuredocs.org/clojure.core/upper-case
- Java String '.toLowerCase' metodi: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--