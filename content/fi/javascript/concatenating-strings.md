---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Javascript: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Mikä on merkkijonojen yhdistäminen ja minkä takia ohjelmoijat tekevät sitä? Merkkijonojen yhdistäminen tarkoittaa kahden tai useamman merkkijonon yhdistämistä yhdeksi isommaksi merkkijonoksi. Ohjelmoijat käyttävät tätä tekniikkaa esimerkiksi luodessaan tulostusta tai tallentamalla tietoa eri muodoissa.

## Miten tehdä:

Seuraavassa on esimerkkejä koodista ja syötteestä, jotka näyttävät, kuinka merkkijonoja yhdistetään käyttäen Javascriptia.

```Javascript
let ensimmäinenLause = "Tämä on";
let toinenLause = "esimerkki";
let kokonaislause = ensimmäinenLause + " " + toinenLause;
console.log(kokonaislause);
```

Tuloste: `Tämä on esimerkki`

Toisessa esimerkissä yhdistämme kaksi numeroa ja merkkijonoa:

```Javascript
let numero = 10;
let merkkijono = "on paljon";
let kokonaislause = "Lisää " + numero + " vuotta ja " + merkkijono
console.log(kokonaislause);
```

Tuloste: `Lisää 10 vuotta ja on paljon`

## Syventävä sukellus:

Merkkijonojen yhdistäminen on ollut käytössä lähes alusta asti Javascriptissa. Aikaisemmin tätä tehtiin käyttämällä konkatenointioperaattoria `+`, mutta nykyään on olemassa myös uusi `template literals` -tapoja tehdä sama asia.

Toinen tapa yhdistää merkkijonoja on käyttämällä Javascriptin `join()`-funktiota, joka yhdistää useita merkkijonoja tiettyyn erottimeen. Esimerkiksi:

```Javascript
let lista = ["omena", "banaani", "appelsiini"];
let uusiMerkkijono = lista.join(", ");
console.log(uusiMerkkijono);
```

Tuloste: `omena, banaani, appelsiini`

## Katso myös:

Lisätietoja merkkijonojen yhdistämisestä ja Javascriptin join()-funktiosta löytyy seuraavista lähteistä:

- [MDN Web Docs: String concatenation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [MDN Web Docs: Template literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- [MDN Web Docs: Array.prototype.join()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/join)