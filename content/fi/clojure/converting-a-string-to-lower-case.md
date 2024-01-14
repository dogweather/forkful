---
title:                "Clojure: Merkkijonon muuntaminen pienaakkosiksi"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi: Miksi muuttaa merkkijono pienaakkosiksi? 

Converting a string to lower case can be useful for data cleaning, text processing, and ensuring consistency in your code. When working with strings, it is important to have a standardized format to avoid errors and make comparisons easier. 

## Kuinka: 

```Clojure 
(println (lower-case "KISSA"))
 ; tulostaa "kissa" 
(println (lower-case "Hei, minkälaista päivää sinulla on?"))
 ; tulostaa "hei, minkälaista päivää sinulla on?" 
``` 

Voit muuntaa merkkijonon pienaakkosiksi käyttämällä `lower-case` -funktiota ja antamalla haluamasi merkkijonon parametriksi. Tämä muuttaa kaikki merkkijonon kirjaimet pienaakkosiksi ja palauttaa uuden merkkijonon. Voit käyttää tätä muodostaaksesi tasalaatuisen vertailupohjan tai helpottamaan tietojen käsittelyä. 

## Syvällinen sukellus: 

Pienaseksuksen muuttaminen on standardiksi monissa ohjelmointikielissä, joten on hyödynnällistä olla tutustunut tähän käsitteeseen. Voit myös halutessasi muokata tekstiä ennen sen passausta algoritmeille tai saadaksesi visuaalisesti yhtenäisen lopputuloksen. Huomioi, että tämä funktio toimii vain ascii-kirjanmerkeille, joten se ei välttämättä toimi tukemme kielelle. Voit kuitenkin koodaamalla omaa funktiota laajentaa näitä toiminnallisuuksia. 

## Katso myös: 

- [Clojure Dokumentaatio](https://clojure.org/reference/strings) 
- [Miten muunnetaan merkkijono pienaakkosiin Javassa](https://www.baeldung.com/java-string-lowercase) 
- [Piena