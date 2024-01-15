---
title:                "Mallin mukaisia merkkejä poistaminen"
html_title:           "Clojure: Mallin mukaisia merkkejä poistaminen"
simple_title:         "Mallin mukaisia merkkejä poistaminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmoijana joudut käsittelemään tekstimuotoista dataa, ja saattaa olla tarpeellista poistaa tietyt merkit tai merkkijonot tietystä tekstistä. Clojure tarjoaa helpon ja tehokkaan tavan poistaa merkkijonoja ja merkkejä, mikä voi säästää paljon aikaa ja vaivaa.

## Näin teet sen

```Clojure 
;; Määritellään apufunktio "poista-merkki", joka poistaa yhden merkin merkkijonosta (str)
(defn poista-merkki [str merkki]
  (apply str (remove #(= % merkki) str)))

;; Käyttökelpoinen silloin, kun halutaan poistaa vain tietty merkki merkkijonosta
(poista-merkki "Tervetuloa!" \!)
;; Output: "Tervetuloa"

;; Jos halutaan poistaa useita merkkejä, voidaan käyttää "reduce"-funktiota
(reduce poista-merkki "Tervetuloa!" [\! \? \.])
;; Output: "Tervetuloa"

;; Samalla logiikalla voidaan poistaa myös merkkijonoja
(reduce poista-merkki "Tervetuloa kaikille!" [" kaikille" "Tervetuloa "])
;; Output: "!"

```

## Syvempi sukellus

Clojuren "remove"-funktio ottaa kaksi argumenttia: predikaatin ja listan. Se palauttaa uuden listan, jossa ovat kaikki alkuperäisen listan arvot poislukien ne, jotka vastaavat predikaattia. "remove" ei muuta alkuperäistä listaa, vaan palauttaa aina uuden listan. Tämä on tärkeää muistaa, sillä esimerkiksi yllä olevassa koodissa käytetty "apply" -funktio hyväksyy vain yhden argementin, joten "remove"-funktiota tulee käyttää poistamaan vain yksi merkki kerrallaan.

## Katso myös

- [Clojure dokumentaatio "remove"-funktiosta](https://clojuredocs.org/clojure.core/remove)
- [Opas Clojuren käyttöön tekstimuotoisen datan käsittelyssä](https://clojure.org/guides/data_manipulation#general_string_manipulation)