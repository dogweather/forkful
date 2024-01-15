---
title:                "Alaotsikko: Alimerkkijonojen erottaminen"
html_title:           "Javascript: Alaotsikko: Alimerkkijonojen erottaminen"
simple_title:         "Alaotsikko: Alimerkkijonojen erottaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi käyttää substringien etsimistä?

Substringien etsiminen on hyödyllinen työkalu, jolla voidaan käsitellä merkkijonoja ja saada tarkkaa dataa halutusta kohdasta. Se on erityisen hyödyllinen silloin, kun käsiteltäviä merkkijonoja on paljon ja halutaan tehdä tarkkoja hakuja ja muokkauksia.

## Kuinka tehdä substringien etsiminen?

```Javascript
// Määritellään merkkijono
let teksti = "Tämä on esimerkkilause";

// Etsitään alkuindeksin 5 jälkeen olevista merkeistä 10 substrin pituista osaa
let osa = teksti.substring(5, 15);
console.log(osa);
// Output: on esimerk

// Haku alkaen alusta ilman lopetusindeksiä
let osa2 = teksti.substring(5);
console.log(osa2);
// Output: on esimerkkilause
```

Tässä esimerkissä määritellään ensin merkkijono ja sen jälkeen käytetään ```substring()```-funktiota hakuun. Funktio ottaa kaksi parametria: alkuindeksin ja lopetusindeksin. Voit myös jättää lopetusindeksin pois, jolloin haku alkaa alusta ja jatkuu loppuun asti.

## Syvempi sukellus substringien etsimiseen

Funktion sisällä tapahtuu muutama asia ennen kuin se palauttaa halutun substringin. Ensinnäkin, merkkijonon indeksit alkavat aina 0:sta, joten ensimmäinen merkki on indeksissä 0. Toiseksi, lopetusindeksi määrittää millä merkillä haku päättyy, mutta tätä merkkiä ei oteta mukaan osaksi palautettua substringia.

Toinen tärkeä huomioitava asia on, että ```substring()```-funktio ei muuta alkuperäistä merkkijonoa, vaan palauttaa uuden merkkijonon. Tämä yhdessä ```let```-avainsanan kanssa mahdollistaa merkkijonon muokkauksen ilman että alkuperäistä dataa menetetään.

## Katso myös

- [String.substring() - MDN web docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [Substring in JavaScript: Four Solutions to Find and Remove Substrings Inside a String](https://codeburst.io/substring-in-javascript-four-solutions-to-find-and-remove-substrings-b69c799f03b8)
- [10 Common String Operations in JavaScript](https://dmitripavlutin.com/10-common-string-operations-in-javascript/)