---
title:                "Satunnainen lukujen generointi."
html_title:           "TypeScript: Satunnainen lukujen generointi."
simple_title:         "Satunnainen lukujen generointi."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

#Mikä & Miksi?

Satunnaisten numeroiden generoiminen tarkoittaa satunnaisen numeron tuottamista koodilla. Koodareille tämä on tärkeää esimerkiksi pelien kehittämisessä, jolloin tarvitaan arvontoja esimerkiksi pelin tapahtumien määrittelemiseen.

#Kuinka:

Käyttämällä TypeScriptin sisäänrakennettua Math.random() -funktiota, joka palauttaa luvun väliltä 0-1, voimme luoda satunnaisia numeroita omassa koodissamme. Esimerkiksi seuraavassa koodinpätkässä luodaan satunnainen luku väliltä 1-10:

```TypeScript
let randomNumber = Math.floor(Math.random() * 10) + 1;
console.log(randomNumber); //tulostaa esimerkiksi luvun 5
```

#Syvemmälle:

Satunnaisia numeroita on käytetty ohjelmoinnissa jo pitkään ja ne ovat tärkeä osa monia sovelluksia ja pelejä. Aikaisemmin satunnaisuuden luomista varten on käytetty esimerkiksi viidennöissuunnattuja muuttujia, mutta nykyaikaiset ohjelmointikielet kuten TypeScript tarjoavat valmiin ratkaisun satunnaisien numeroiden generoimiseen. Lisäksi on olemassa myös muita algoritmeja ja kirjastoja, kuten Random.js, jotka tarjoavat erilaisia vaihtoehtoja satunnaisien numeroiden generointiin.

#Katso myös:

- [TypeScriptin virallinen dokumentaatio Math-objektista](https://www.typescriptlang.org/docs/handbook/numbers.html#math)
- [Random.js-kirjaston GitHub-sivu](https://github.com/ckknight/random-js)