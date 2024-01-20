---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "Go: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Merkkijonojen pituuden määrittäminen TypeScriptissä tarkoittaa merkkien lukumäärän laskemista merkkijonossa. Tämä on oleellista, kun haluamme esimerkiksi käsitellä merkkijonoa osissa tai verrata eri merkkijonojen pituuksia.


## Miten toimia:

Voit saada merkkijonon pituuden TypeScriptissä käyttämällä `.length` ominaisuutta. Tässä on esimerkki:

```TypeScript
let sana: string = "Moi maailma";
console.log(sana.length); // Tulostaa: 11
```

Yllä olevassa koodissa `sana` on merkkijono, ja `sana.length` palauttaa merkkijonojen määrän, joka tässä tapauksessa on 11.


## Syvempi sukellus:

Merkkijonon pituuden määrittäminen on ollut perusominaisuutena monissa ohjelmointikielissä vuosikymmenten ajan. TypeScriptissä, niin kuin monissa muissakin nykykielissä, on tämä ominaisuus sisäänrakennettuna, eli ei tarvitse käyttää mitään ulkopuolista funktiota.

On olemassa myös vaihtoehtoisia tapoja laskea merkkijonon pituus, kuten `.split('')` ja `.join('')` metodien käyttö, mutta `.length` on tehokkain ja suoraviivaisin tapa.

Merkkijonon pituuden määrittämisen yksityiskohtaisempi toteutus riippuu itse kielestä. TypeScriptissä `.length` ominaisuus palauttaa UTF-16 koodausyksiköiden määrän, joka ei välttämättä vastaa merkkien määrää jos merkkijonossa on esimerkiksi emoji, joka vaatii kaksinkertaisen koodausyksikön.


## Katso myös:

- MDN Web Docs, String.length: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length
- TypeScriptin virallinen dokumentaatio: https://www.typescriptlang.org/docs/