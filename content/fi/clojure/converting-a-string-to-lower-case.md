---
title:                "Merkkijonon muuntaminen pienaakkosiksi"
html_title:           "Clojure: Merkkijonon muuntaminen pienaakkosiksi"
simple_title:         "Merkkijonon muuntaminen pienaakkosiksi"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi ja miten muuntaa merkkijono pieniksi kirjaimiksi Clojurella

## Miksi ja miten muuntaa merkkijono pieniksi kirjaimiksi?

Merkkijonon muuttaminen pieniksi kirjaimiksi tarkoittaa, että kaikki merkit muutetaan pieniksi kirjaimiksi, jotta niitä voi käsitellä yhtenä kokonaisuutena. Tämä on tärkeää esimerkiksi hakutoiminnoissa, jolloin pienet ja isot kirjaimet eivät vaikuta tuloksiin. Ohjelmoijat käyttävät tätä toimintoa usein helpottaakseen merkkijonojen vertailua ja muokkaamista.

## Miten tehdä se Clojurella:

```Clojure
(.toLowerCase "HELLO WORLD")
; => "hello world"
```

```Clojure
;; Voit myös käyttää case stamenttia:
(case "Hello World"
  "Hello" "hello"
  "World" "world")
; => "hello world"
```

## Syvempi katsaus

Historiallisessa kontekstissa, merkkijonon muuttaminen pieniksi kirjaimiksi on ollut tärkeää erityisesti ennen unicodea, jolloin kirjaimilla oli rajoitettu määrä erilaisia merkkejä ja koodaustyylejä. Nykyään sillä on edelleen merkitystä erityisesti tietokannoissa ja datan käsittelyssä.

Vaihtoehtoisia tapoja muuntaa merkkijono pieniksi kirjaimiksi ovat esimerkiksi säännölliset lausekkeet ja string-funktiot kuten `substring` ja `replace`. Näitä ei kuitenkaan ole suunniteltu nimenomaan merkkijonon koon muuttamiseen, joten ne eivät välttämättä ole yhtä tehokkaita.

Taustalla oleva Clojure-komento `toLowerCase` käyttää Java-standardikirjaston `String`-luokkaa, joten sen tarkempi toteutus riippuu käytettävästä Java-ympäristöstä. Se mahdollistaa myös argumenttina annetun merkkijonon muiden kirjainten muuttamisen pieniksi kirjaimiksi oikeilla kieliasetuksilla.

## Katso myös

[Clojure-dokumentaatio Java String-luokasta](https://clojuredocs.org/clojure.core/to-lower-case#example-572a4579e4b0059e0ea8d664)