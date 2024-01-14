---
title:                "Javascript: Alimerkkien erottaminen"
simple_title:         "Alimerkkien erottaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi: Miksi haluaisin poimia alimerkkejä (substring)?

Kun työskentelet tekstin käsittelyssä, saatat törmätä tilanteeseen, jossa haluat tarkistaa tai muokata tietyn osan tekstistä. Tässä tilanteessa alimerkkien (substring) poimiminen on hyödyllinen ominaisuus, joka säästää aikaa ja vaivaa.

Esimerkiksi haluat ehkä tarkistaa, onko tietyssä sanassa tietty kirjain tai haluatko jakaa tekstin eri osiin. Alimerkkien poimiminen antaa sinulle mahdollisuuden työskennellä tarkasti halutun tekstin kanssa helposti ja tehokkaasti.

## Kuinka: Näin poimit alimerkkiä käyttäen JavasScriptiä

Alimerkkien poimiminen JavaScriptillä on melko helppoa. Voit käyttää String-olioon sisäänrakennettua "substring" -toimintoa. Se ottaa parametreikseen aloitus- ja lopetusindeksit ja palauttaa halutun osan tekstin alimerkkinä.

```Javascript
let teksti = "Tämä on esimerkki tekstistä."
let osa = teksti.substring(5, 17)
console.log(osa) // "on esimerkki"
```

Tässä esimerkissä "substring" -toiminto poimii tekstin osan, joka alkaa indeksistä 5 ja päättyy indeksiin 17. On tärkeää huomata, että aloitusindeksi on mukana, mutta lopetusindeksi ei.

Voit myös asettaa vain yhden parametrin, jolloin "substring" -toiminto palauttaa kaiken tekstin aloittaen halutusta indeksistä.

```Javascript
let teksti = "Tämä on esimerkki tekstistä."
let osa = teksti.substring(10)
console.log(osa) // "esimerkki tekstistä."
```

## Syvällinen sukellus

Alimerkkien poimimiseen liittyy muutamia tärkeitä seikkoja, jotka on hyvä pitää mielessä.

Ensinnäkin, kun poimit alimerkkiä, muista, että tekstin indeksointi alkaa aina 0: sta. Toiseksi, "substring" -toiminto ei muuta alkuperäistä tekstiä, vaan palauttaa uuden tekstinäyttövastauksen. Kolmanneksi, alimerkin pituuden on oltava suurempi tai yhtä suuri kuin alkuperäisen tekstin. Muuten palautetaan tyhjä vastaus.

Lisäksi, jos haluat puuttua tekstistä merkkeihin, voit käyttää "charCodeAt" -toimintoa, mikä antaa sinulle merkin UTF-16 Unicode -arvon. Tämä mahdollistaa tarkemman tekstin käsittelyn.

## Katso myös

- [MDN Web Docs - substring](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [W3Schools - JavaScript substring() Method](https://www.w3schools.com/jsref/jsref_substring.asp)
- [Ultimate Guide to JavaScript Substrings](https://www.javascripttutorial.net/javascript-substring/)