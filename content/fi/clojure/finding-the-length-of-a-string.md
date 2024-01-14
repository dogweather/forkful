---
title:                "Clojure: Merkkijonon pituuden löytäminen"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, joissa programmeraaja tarvitsee selvittää merkkijonon pituus Clojure-kielellä. Tämä voi sisältyä tekstianalyysiin tai datan käsittelyyn. Merkkijonojen pituuden laskeminen on tärkeä perustaito, joka helpottaa monia ohjelmointitehtäviä Clojuren avulla.

## Miten

Merkkijonon pituuden laskeminen Clojurella on helppoa. Tämä voidaan tehdä käyttämällä "count" -funktiota, joka palauttaa merkkien määrän parametrina annetussa merkkijonossa.

```Clojure
(count "Tämä on esimerkki") ; tulostaa 18
```

"Count" -funktio toimii myös listoille ja muille kokoelmille. Se palauttaa kaikkien kokoelman alkoiden määrän.

```Clojure
(count [1 2 3 4 5]) ; tulostaa 5
(count "Clojure") ; tulostaa 7
```

## Syvempi sukellus

Merkkijonojen pituuden laskeminen Clojurella perustuu Unicode-merkkien määrään. Tämä tarkoittaa, että jokainen merkki lasketaan, riippumatta sen leveydestä tai monimutkaisuudesta. Tämä eroaa joistakin muista ohjelmointikielistä, joissa jotkut merkit voivat vaatia useampia tavuja ja siten vaikuttaa laskettuun pituuteen.

On myös huomattava, että "count" -funktio saattaa olla hidas suurille merkkijonoille, koska se käy läpi jokaisen merkin. Tämän vuoksi, jos olet huolissasi suorituskyvystä, sinun kannattaa harkita erilaisten optimointistrategioiden käyttöä merkkijonojen pituuden laskemiseksi.

## Katso myös

- [Clojure - String Functions](https://clojure.org/api/cheatsheet#String%20Functions)
- [ClojureDocs - count function](https://clojuredocs.org/clojure.core/count) 
- [Unicode and Character Representation in Clojure](https://doraboda.wordpress.com/2019/03/28/unicode-and-character-representation-in-clojure/)