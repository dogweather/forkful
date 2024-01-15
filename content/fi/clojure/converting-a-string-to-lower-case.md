---
title:                "Muuntaminen merkkijonoksi pienellä kirjaimella"
html_title:           "Clojure: Muuntaminen merkkijonoksi pienellä kirjaimella"
simple_title:         "Muuntaminen merkkijonoksi pienellä kirjaimella"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi
Hieman yli vuosi sitten Clojuren uusin versio 1.10 julkaistiin. Tämän päivityksen myötä eräs hyödyllinen ominaisuus lisättiin Clojureen: kyky muuttaa merkkijono pienaakkosiksi. Tässä artikkelissa tarkastelemme tarkemmin, miksi tämä ominaisuus on niin hyödyllinen ja miten sitä voi hyödyntää.

## Miten tehdä
Merkkijonon muuttaminen pienaakkosiksi on yksinkertaista Clojuressa. Käytämme tähän ```clojure/format``` kirjastoa, joka tarjoaa toiminnallisuuden muuttaa merkkijono pienaakkosiksi. Katso esimerkki alla:

```clojure
(require '[clojure.format :refer [lower-case]])
(lower-case "MOI MAAILMA!")
```

Tämä palauttaa tuloksena merkkijonon "moi maailma!".

## Syvällisempi tarkastelu
Yksi syy miksi tämä ominaisuus on hyödyllinen on se, että se helpottaa merkkijonojen vertailua. Pienaakkosiksi muuttaminen mahdollistaa samojen merkkijonojen vertailun, vaikka ne olisivat eri muodossa. Esimerkiksi tyypillinen tapaus voi olla tarkistaa onko käyttäjän syöttämä sana sama kuin tallennettu käyttäjänimi, eikä pienaakkosilla ole tässä tapauksessa merkitystä.

Lisäksi pienaakkosiksi muuttaminen on myös hyödyllistä silloin, kun käsittelemme merkkijonoja tietokannassa tai tiedostoissa. Esimerkiksi jos haluamme hakea tietokannasta tietyn käyttäjän tiedot käyttäjänimen perusteella, on helposti määriteltävä käyttäjänimen olevan pienaakkosilla tai ilman.

## Katso myös
- [Clojuren uusin versio 1.10](https://github.com/clojure/clojure/releases/tag/clojure-1.10.0)
- [Clojuren format-kirjasto](https://clojure.github.io/clojure/clojure.format-api.html#clojure.format/lower-case)