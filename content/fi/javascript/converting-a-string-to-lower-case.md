---
title:    "Javascript: Merkkijonon muuttaminen pienaakkosiksi"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

JavaScriptissä on usein tarve muuttaa merkkijono pieniksi kirjaimiksi, esimerkiksi siksi että halutaan vertailla kahta samankaltaista merkkijonoa. Tämä voidaan tehdä käyttämällä merkkijonon lower case versiota.

## Miten

Merkkijonon muuttaminen pieniksi kirjaimiksi voidaan tehdä yksinkertaisesti käyttämällä `toLowerCase()` metodia. Seuraava esimerkki näyttää miten tämä voidaan tehdä:

```javascript
let string = "TÄMÄ ON ESIMERKKI";
console.log(string.toLowerCase());
```

Tulosteena saadaan `tämä on esimerkki`. Huomaa, että metodi ei muuta alkuperäistä merkkijonoa, vaan palauttaa uuden merkkijonon.

## Syväsukellus

Merkkijonon muuttaminen pieniksi kirjaimiksi tapahtuu taustalla käyttäen Unicode-standardin `toLowerCase` funktiota. Tämä tarkoittaa, että vaikka tämä toimii suurimmalle osalle merkkijonoista, on olemassa poikkeustapauksia. Tässä pari esimerkkiä:

1. Joissain kielissä, kuten saksassa, joillain kirjaimilla on kaksi eri pienikirjain versiota. Esimerkiksi ß voidaan muuttaa joko ss tai ß. Tämä saattaa aiheuttaa ongelmia, koska `toLowerCase` metodi ei osaa arvioida kumpi versio on oikein.
2. Joissain kiinalaisissa merkistöissä ei ole käsitettä pieniä ja suuria kirjaimia, joten metodi ei tee mitään muutoksia.

Näiden poikkeustapausten huomioiminen on tärkeää erityisesti silloin kun käytetään merkkijonon vertailua.

## Katso myös

- [Unicode standardi](https://unicode.org/)
- [String.toLowerCase() MDN dokumentaatio](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)