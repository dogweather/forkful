---
title:                "Clojure: Tietokoneohjelmoinnin kirjoittaminen standardivirheeseen"
simple_title:         "Tietokoneohjelmoinnin kirjoittaminen standardivirheeseen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kirjoittaisit standardivirheeseen (standard error)? Jos haluat tallentaa virheilmoitukset, jotka voivat auttaa sinua löytämään ja korjaamaan ohjelmointivirheitä. Tämä on hyödyllinen vianetsintätyökalu, joka auttaa sinua ymmärtämään, mistä ohjelmassasi havaitut virheet johtuvat.

## Kuinka tehdä

Standardivirheeseen kirjoittaminen Clojuressa on yksinkertaista. Voit käyttää "System/err" toimintoa ja antaa sille haluamasi virheilmoituksen merkkijonona. Tämän jälkeen ohjelma tallentaa virheilmoituksen standardivirheeseen sen sijaan, että tulostaisi sen normaalisti.

```Clojure
(System/err "Tämä on virheilmoitus, joka tallennetaan standardivirheeseen")
```

Tässä on esimerkki tulosteesta, jonka saat, kun suoritat tämän koodin:

```Clojure
Tämä on virheilmoitus, joka tallennetaan standardivirheeseen
```

## Syvemmälle

On tärkeää huomata, että standardivirheeseen kirjoittaminen voi olla hyödyllistä vain silloin, kun ohjelma ei pysty jatkamaan normaalisti virheen jälkeen. Jos ohjelmasi kuitenkin kykenee jatkamaan, voi olla parempi käyttää "System/out" toimintoa, joka tallentaa tulosteen standarditulosteeseen.

Kannattaa myös huomata, että standardivirheen sisältöä voidaan lukea ja käsitellä myös ohjelmassa. Voit käyttää "System/err-reader" -toimintoa ja "read-line" -toimintoa lukeaksesi standardivirheeseen tallennetun virheilmoituksen merkkijonona.

## Katso myös

- [Clojure.org: I/O](https://clojure.org/reference/io)
- [System/err -toiminto](https://clojuredocs.org/clojure.java.io/system%2Ferr)
- [Standard Input ja Output tulostusten ohjaaminen](https://clojure.org/reference/io#_redirecting_standard_input_and_output)