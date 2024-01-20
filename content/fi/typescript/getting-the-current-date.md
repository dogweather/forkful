---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "Haskell: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

# TypeScriptilla Päivämäärän Haaliminen: Nopea Opas

## Mikä & Miksi?

Päivämäärän haaliminen TypeScriptilla tarkoittaa nykyisen päivämäärän ja ajan hakemista sovellukselle. Tämä on välttämätöntä sikäli kun sovellukset tarvitsevat reaaliaikaista dataa toimiakseen asianmukaisesti, esimerkiksi ajan leimaukseen tai aikaperusteiseen toiminnallisuuteen.

## Kuinka:

Voit saada nykyisen päivämäärän TypeScriptilla `Date`-objektin `ìnstanceof`-metodilla. Tavallisesti koodi näyttää tältä.

```TypeScript
let currentDate = new Date();
console.log(currentDate);
```

Kun suoritat tämän koodin, saat ulostulona nykyisen päivämäärän ja ajan, esimerkiksi: `2021-08-31T12:00:00.000Z`

## Syvempi Tutkailu

JavaScriptin `Date`-objekti on peräisin ECMAScript-standardista, ja se on TypeScriptin ajurityyppi nykyisen päivämäärän noutamiseksi. Vaihtoehtoina voit käyttää monikäyttöisiä kirjastoja, kuten Moment.js luodaksesi monimutkaisempia aikatoimintoja.

Nykyisen päivämäärän noutaminen on varsin yksinkertainen operaatio. `Date`-objekti luo päivämäärä- ja aika-arvojen yhdistelmän, mukaan lukien päivän, kuukauden, vuoden, tunnin, minuutin, sekunnin ja millisekunnin. 

Voit käyttää `Date`-objektin metodeja nykyisen päivämäärän muokkaamiseen tai tiettyjen komponenttien, kuten päivämäärän, kuukauden tai vuoden, noutamiseen.

## Katso myös

- [Working with Dates - MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)