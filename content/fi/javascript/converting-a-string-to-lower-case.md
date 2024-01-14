---
title:                "Javascript: Merkkijonon muuttaminen pienaakkosiksi"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi:

Monet JavaScript-ohjelmoijat haluavat muuttaa merkkijonon pienaakkosiin erilaisten syiden vuoksi, kuten tasaamaan tekstien vertailun ja helpottamaan hakutoimintoja. Tämä kirjoitus opastaa kuinka ja miksi muuntaa merkkijono pienaakkosiin.

## Miten:

Merkkijonon muuttaminen pienaakkosiin on yksinkertaista käyttäen JavaScriptin toimintoa `toLowerCase()`. Tässä on yksinkertainen esimerkki:

```Javascript
var teksti = "TÄMÄ ON MERKKIJONO SUURAUKKOSILLA";
var uusiTeksti = teksti.toLowerCase();
console.log(uusiTeksti);
```

Tuloste:

```Javascript
tämä on merkkijono suuraakkosilla
```

Tässä esimerkissä ensin luodaan uusi muuttuja `teksti`, jossa on merkkijono suuraakkosilla. Sitten käytetään `toLowerCase()` -toimintoa luomaan uusi muuttuja `uusiTeksti`, joka on alkuperäisen merkkijonon pienaakkosversio. Lopuksi tulostetaan uusi muuttuja `console.log()`-komennolla.

## Syvällisempi sukellus:

`toLowerCase()` -toiminto muuntaa kaikki merkkijonon kirjaimet pienaakkosiksi. Tämä on kätevää esimerkiksi kun halutaan verrata kahta merkkijonoa, sillä pienet ja suuret kirjaimet eivät vaikuta vertailun tulokseen. Lisäksi se helpottaa hakutoimintoja, sillä pienaakkoset ja suuraakkoset katsotaan yleensä samaksi merkiksi.

On myös hyvä huomata, että `toLowerCase()` -toiminto ei muuta alkuperäistä merkkijonoa, vaan luo uuden. Tästä syystä uusi versio täytyy tallentaa omaan muuttujaan. Lisäksi se ei vaikuta erikoismerkkeihin tai muihin kuin kirjaimiin kuuluviin merkkeihin, vaan muuttaa ainoastaan aakkoset pienaakkosiksi.

## Katso myös:

- [String toLowerCase() -toiminnon dokumentaatio](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Miten vertailla merkkijonoja JavaScriptissä](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/localeCompare)

Kiitos lukemisesta! Toivottavasti tämä opas auttoi sinua ymmärtämään merkkijonon muuntamista pienaakkosiin JavaScriptissä. Tärkein pointti on muistaa tallentaa uusi versio omaan muuttujaansa, sillä alkuperäinen merkkijono ei muutu. Hyödynnä tätä toimintoa seuraavan koodiasi kirjoittaessasi ja tulet huomaamaan sen hyödyt!