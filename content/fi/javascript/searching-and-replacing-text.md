---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Javascript: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Textin etsiminen ja korvaaminen on olennainen osa Javascript-ohjelmointia. Se auttaa kehittäjiä muokkaamaan ja hallitsemaan tekstejä nopeasti ja tehokkaasti.

## Miten

Javascript tarjoaa useita erilaisia tapoja etsiä ja korvata tekstiä. Yksi yleisimmistä on käyttää tekstiin liittyviä metodeja, kuten `replace()`. Tässä esimerkissä haluamme korvata kaikki kirjaimet "a" kirjaimella "b":

```Javascript
let teksti = "Tämä on esimerkki";
let uusiTeksti = teksti.replace(/a/g, "b");
console.log(uusiTeksti);

// Output: Tbmb on esimerkki
```

Voimme myös käyttää säännöllisiä lausekkeita etsimään ja korvaamaan tekstiä tietyin kriteerein. Esimerkiksi haluamme poistaa kaikki numerot tekstistä:

```Javascript
let teksti = "2 h Song";
let uusiTeksti = teksti.replace(/[0-9]/g, "");
console.log(uusiTeksti);

// Output: h Song
```

## Syväsukellus

Jos haluamme tehdä monimutkaisempia etsimis- ja korvaamistehtäviä, voimme käyttää `RegExp`-olioita. Ne antavat meille enemmän kontrollia etsinnöissä ja mahdollistavat esimerkiksi tietyn tekstin osan tallentamisen muuttujaksi. Esimerkiksi haluamme etsiä ja korvata kaikki kassalla olevat tuotteet tekstissä:

```Javascript
let teksti = "Kassalla oli 5 tuotetta";
let haettava = /(\d+)\stuotetta/;
let korvaus = "$1 000 tuotetta";
let uusiTeksti = teksti.replace(haettava, korvaus);
console.log(uusiTeksti);

// Output: Kassalla oli 5 000 tuotetta
```

Voimme myös käyttää `replace()`-menetelmän sijasta `replaceAll()`-menetelmää, joka korvaa kaikki löydetyt tekstit kerralla ilman säännöllistä lauseketta. Esimerkiksi haluamme korvata jokaisen numeron sitä edeltävällä tekstillä:

```Javascript
let teksti = "Luvut 1, 2 ja 3 ovat hyvin tärkeitä";
let uusiTeksti = teksti.replaceAll(/\d/g, "python");
console.log(uusiTeksti);

// Output: Luvut python, python ja python ovat hyvin tärkeitä
```

## Katso myös

- RegExp-olio: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp
- replace()-metodi: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- replaceAll()-metodi: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replaceAll