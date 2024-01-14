---
title:    "Javascript: Merkkijonon pituuden löytäminen"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi 

Usein käytetyssä ohjelmointikielessä, kuten Javascript, on tärkeää tietää, kuinka monta merkkiä tai kirjainta on tietyssä merkkijonossa. Tämä tieto voi olla hyödyllinen monissa eri tilanteissa, kuten tietojen validoinnissa tai tekstin muotoilussa. Siksi on tärkeää tietää, kuinka löytää merkkijonon pituus käyttäen Javascriptiä.

## Kuinka tehdä se 

Merkkijonon pituuden löytäminen Javascriptissä on yksinkertaista. Voit käyttää `.length` -metodia merkkijonoon, joka palauttaa merkkijonon pituuden numerona. Katso esimerkki alla olevasta koodista:

```Javascript
let merkkijono = "Tervetuloa lukemaan ohjelmointiblogia!";
console.log(merkkijono.length);
```

Tuloste: `35`

Lisäksi voit käyttää `length` -metodia myös muihin tietotyyppeihin, kuten taulukoihin, objekteihin ja jopa numeromuuttujiin. Tämä metodi on siis erittäin hyödyllinen ja monipuolinen työkalu.

## Syvällisempi sukellus 

Merkkijonon pituuden löytäminen `.length` -metodilla perustuu Unicode-koodipisteiden lukumäärään. Unicode-koodipisteet ovat numerokoodeja, jotka liitetään jokaiseen kirjaimen tai merkin esitystapaan tietokoneella. Koska jotkut merkit vaativat useampia kuin yhden koodipisteen, merkkijonon pituus voi vaihdella merkistöstä riippuen. Esimerkiksi "ä" -merkki voi vaatia kaksi koodipistettä, kun taas "a" -merkki vaatii vain yhden.

Lisäksi, `.length` -metodi ei pysty laskemaan virtuaalisia merkkejä, joita käytetään esimerkiksi joidenkin aasialaisten kielten kirjoittamisessa. Tässä tapauksessa voit käyttää `.padEnd()` ja `.padStart()` -metodeja laskemaan todellinen merkkijonon pituus.

## Katso myös 

- [Javascriptin virallinen dokumentaatio] (https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Koodin esimerkki] (https://developer.mozilla.org/fi/docs/Web/JavaScript/Guide/Global_Objects/String/length_examples)