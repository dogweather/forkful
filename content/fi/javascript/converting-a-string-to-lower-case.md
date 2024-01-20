---
title:                "Merkkijonon muuttaminen pieniksi kirjaimiksi"
html_title:           "Gleam: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tekstijonojen muuttaminen pieniksi kirjaimiksi tarkoittaa, että kaikki merkkijonon isot kirjaimet muunnetaan vastaaviksi pieniksi kirjaimiksi. Sitä käytetään usein vertailemaan tekstijonoja riippumatta siitä, kirjoitetaanko ne suurilla vai pienillä kirjaimilla.

## Näin teet: 

Javascriptissa voit muuttaa merkkijonon pieniksi kirjaimiksi käyttämällä `toLowerCase()`-metodia. Tässä on esimerkki:

```Javascript
let teksti = "MoikkA Suomi!";
let pienet = teksti.toLowerCase();

console.log(pienet);  // tulostaa: "moikka suomi!"
```

## Syvä sukellus

`toLowerCase()`-metodi on ollut osa Javascriptiä sen varhaisista versioista lähtien, ja se on standardi tapa saada tekstijono muutettua pieniksi kirjaimiksi. Jokaista merkkijonon kirjainta käsitellään erikseen, mikä tekee metodista luotettavan erilaisissa tilanteissa. 

Vaihtoehtoisesti, jos tarvitset toimintaa, joka on herkkä kielikohtaisille yksityiskohdille, voit käyttää `toLocaleLowerCase()`-metodia. Se toimii samalla tavalla, mutta ottaa huomioon sijainnin (locales) asetukset, mikä on hyödyllistä kielissä, joissa on erikoismerkkejä.

```Javascript
let teksti = "Äiti!";
let pienet = teksti.toLocaleLowerCase('tr-TR');  // Tässä käytetään Turkin kielen asetuksia

console.log(pienet);  // tulostaa: "äiti!"
```

Vaikka nämä metodit ovat hyvin tehokkaita ja helppokäyttöisiä, niiden takana on paljon monimutkaista logiikkaa. Esimerkiksi, ne pitävät sisällään Unicode-taulukot, jotka määrittelevät, miten jokainen suuri kirjain muunnettaisiin vastaavaksi pieneksi kirjaimeksi eri kieliympäristöissä.

## Katso myös

Jotta voisit syventää ymmärrystäsi merkkijonojen käsittelystä Javascriptissa, tutustu seuraaviin lähteisiin:

* MDN:n [toLowerCase()](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase) ja [toLocaleLowerCase()](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase) -metodien dokumentaatio
* [JavaScript String toLowerCase() Method](https://www.w3schools.com/jsref/jsref_tolowercase.asp) W3Schools-sivustolla.