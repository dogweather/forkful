---
title:    "Javascript: Alirivien erottaminen"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi: Miksi perehtyä alimerkkien pilkkomiseen?
Alimerkkien pilkkominen, eli tietyn merkkijonon osan erottaminen ja käyttö erikseen, on tärkeä osa Javascript-ohjelmointia. Se mahdollistaa tietyn tiedon etsimisen ja käyttämisen suuremmasta tekstistä, mikä helpottaa monien ohjelmointitehtävien toteuttamista.

## Kuinka: Koodaesimerkkejä ja tulostuksia ```Javascript ... ``` -lohkoissa
Teemalla "oppiminen tekemällä" esittelemme seuraavassa, kuinka voit käyttää Javascriptin `substring()` -funktiota alimerkkien pilkkomiseen. Esimerkiksi halutessasi etsiä ja käyttää tietyn sanan tai numeron pituutta, voit käyttää alla olevaa koodia:

```Javascript
let sana = "Tämä on esimerkki lause.";
let pituus = sana.substring(11, 18);
console.log(pituus);
```

Tämä koodi tulostaa "esimerkki" konsolille, sillä funktiolla `substring()` haemme merkkijonosta sijainnit 11 ja 18 väliltä. Voit myös käyttää tätä funktiota eri muuttujille ja tulostaa tarvittavan tiedon haluamallasi tavalla.

## Syväsukellus: Tietoa alimerkkien pilkkomisesta
Tämän lisäksi on hyvä huomata, että `substring()` -funktio toimii myös kahdella parametrilla. Jos käytät vain yhden parametrin, se aloittaa hakuoperaation parametrin kohdasta ja etsii loppuun asti. On myös hyvä huomata, että funktio `substr()` toimii samalla tavalla kuin `substring()`, mutta sen ensimmäinen parametri määrittää aloitussijainnin ja toinen parametri halutun merkkimäärän.

## Katso myös
- [MDN web docs - substring()](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [W3Schools - substring() Method](https://www.w3schools.com/jsref/jsref_substring.asp)
- [JavaScript substring() and substr()](https://www.tutorialrepublic.com/javascript-tutorial/javascript-string-substring-methods.php)