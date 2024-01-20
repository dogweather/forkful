---
title:                "Satunnaisten numeroiden luominen"
html_title:           "Bash: Satunnaisten numeroiden luominen"
simple_title:         "Satunnaisten numeroiden luominen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

JavaScriptilla satunnaislukujen tuottaminen auttaa kehittämään ennustamattomia toiminnoja, kuten arpapelit ja ainutlaatuiset tunnisteet. Ohjelmoijat tekevät tämän lisätäkseen sovellusten monimutkaisuutta ja säilyttääkseen käyttäjien kiinnostuksen.

## Näin voit tehdä:

Generoi satunnaisen luvun Javascriptilla seuraavasti:

```Javascript
// Luodaan satunnainen luku väliltä 0 (mukaan lukien) ja 1 (pois lukien)
var random = Math.random();
console.log(random);
```

Tämä palauttaa jotakin kuten `0.2321423278501851`

Jos haluat rajoittaa arvot tiettyyn välille, esimerkiksi 1 ja 10, voit tehdä näin:

```Javascript
// Luodaan satunnainen luku väliltä 1 (mukaan lukien) ja 10 (mukaan lukien)
var random = Math.floor(Math.random() * 10) + 1;
console.log(random);
```

Tämä palauttaa minkä tahansa kokonaisluvun 1 ja 10 väliltä.

## Syvällisempi tarkastelu

JavaScriptin `Math.random()`-metodi perustuu pseudo-satunnaislukugeneraattoriin (PRNG), joka tarkoittaa, että se ei tuota oikeasti "satunnaisia" lukuja, vaan luvut näyttävät olevan satunnaisia.

Muina vaihtoehtoina voimme käyttää muita satunnaislukugeneraattoreita riippuen tarpeistamme, kuten crypto.randomBytes Node.js:ssä turvallisia kryptografisia toimintoja varten.

Se, miten `Math.random()` toteutetaan, vaihtelee eri selainmoottoreissa. JavaScriptin moottorin yksityiskohdat ovat yleensä avoimen lähdekoodin, joten niitä voi tutkia tarkemmin.

## Katso myös

[MDN Web Docs Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)

[Understanding Math.random()](https://medium.com/@betable/tifu-by-using-math-random-f1c308c4fd9d)

[Anatomy of a JavaScript engine](https://en.wikipedia.org/wiki/JavaScript_engine)