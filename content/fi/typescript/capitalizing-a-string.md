---
title:                "Merkkijonon kirjainten muuttaminen isoiksi"
html_title:           "TypeScript: Merkkijonon kirjainten muuttaminen isoiksi"
simple_title:         "Merkkijonon kirjainten muuttaminen isoiksi"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Merkkijonon isoalkukirjoituksella tarkoitetaan sitä, että muutetaan merkkijonon ensimmäinen kirjain isoksi. Ohjelmoijat tekevät tämän usein, kun halutaan parantaa käyttäjäkokemusta tai tehdä tekstistä helpommin luettavaa. 

## Näin se tehdään:

```TypeScript
function teeIsoalkukirjaiminen(merkkijono: string): string {
    return merkkijono.charAt(0).toUpperCase() + merkkijono.slice(1);
}

console.log(teeIsoalkukirjaiminen('hei maailma')); // Tulostaa: 'Hei maailma'
```
Toiminnon takana on se, että otamme merkkijonon ensimmäisen kirjaimen, muutamme sen isoksi ja sitten liitämme lopun merkkijonon.

## Syvempi tarkastelu

Isoalkukirjoituksen historia ulottuu aina kirjoituksen alkuaikoihin asti, ja sen tarkoitus on ollut tehdä tekstistä selkeää ja ymmärrettävää. TypeScriptissä ei ole sisäänrakennettua funktionaalisuutta merkkijonon ensimmäisen kirjaimen muuttamiseksi isoksi, joten yleisesti käytetty tapa on yllä kuvattu.

Vaihtoehtoina voitaisiin käyttää kirjastoa, kuten Lodash, mutta useimmissa tapauksissa yksinkertainen funktio toimii yhtä hyvin. Tämän tyyppinen funktio ei vaadi muistia tai laskentatehoa merkittävästi, joten sen toteuttaminen itse on järkevää.

Toteutuksessa yksityiskohta on, että `charAt(0).toUpperCase()` muuttaa ensimmäisen kirjaimen isoksi, mutta mikäli merkkijono alkaa ei-kirjaimellisella merkillä, tämä merkki jätetään siihen sellaisenaan. `slice(1)` ottaa lopun merkkijonosta alkuperäisessä muodossa.

## Katso myös

Lisätietoja ja syventävää materiaalia:

- [MDN web docs, CharAt](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [MDN web docs, Slice](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [MDN web docs, toUpperCase](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [Lodash](https://lodash.com/docs/)