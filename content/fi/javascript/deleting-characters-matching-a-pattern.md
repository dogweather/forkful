---
title:                "Javascript: Hahmon yhdistävien merkkien poistaminen"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointitilanteissa voi olla tarpeellista poistaa merkkejä, jotka vastaavat tiettyä kaavaa tai kriteeriä. Tämä voi johtua tiedon muotoilun tarpeesta, tiettyjen merkkien siivoamisesta tai muista syistä. Seuraavassa jaana käydään läpi Javasript-koodin avulla, kuinka voit helposti poistaa merkkejä vastaavat kaavat tai kriteerit.

## Miten

Käyttämällä `replace()`-funktiota ja säännöllisiä lausekkeita, voit poistaa merkkejä, jotka vastaavat tiettyä kaavaa. Seuraavassa esimerkissä haluamme poistaa kaikki välilyönnit merkkijonosta.

```Javascript
let merkkijono = "Tämä on esimerkki merkkijonosta, jossa on paljon välilyöntejä.";
let uusiMerkkijono = merkkijono.replace(/ /g, "");
console.log(uusiMerkkijono);
```

Tulostaa: "Tämäonesimerkkimerkkijonosta,jospaljonvälilyöntejä."

Tässä esimerkissä `replace()`-funktio korvaa jokaisen välilyönnin merkkijonossa tyhjällä merkkijonolla `""` ja lisää parametrin `g` ansiosta kaikkiin välilyönteihin. Voit myös käyttää muita säännöllisiä lausekkeita ja kaavoja poistaaksesi tiettyjä merkkejä haluamallasi tavalla.

## Syvällinen sukellus

Säännölliset lausekkeet ovat tehokas ja monipuolinen työkalu merkkien ja kaavojen hallintaan. Niitä käytetään usein erilaisissa tietojenmuokkaus- ja tekstinkäsittelytilanteissa. Javasriptissä säännölliset lausekkeet ovat esineitä, joilla on predefinoidut tai käyttäjän määrittelemät kaavat. Ne koostuvat yksinkertaisista merkeistä ja operaattoreista, joiden avulla voit etsiä ja muokata merkkejä haluamallasi tavalla.

Tässä esimerkissä käytimme vain yksinkertaista säännöllistä lauseketta, mutta voit syventää osaamistasi ja löytää monimutkaisempia ja hyödyllisempiä lausekkeita. Säännöllisten lausekkeiden oppiminen auttaa sinua ymmärtämään paremmin, kuinka käsittellä merkkejä ohjelmointikielissä, kuten Javasriptissä.

## Katso myös

- [Javasriptin säännölliset lausekkeet -MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [RegExr - väline säännöllisten lausekkeiden kokeilemiseen ja testaamiseen](https://regexr.com/)