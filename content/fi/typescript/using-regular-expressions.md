---
title:                "Regulaarilausekkeiden käyttö"
html_title:           "TypeScript: Regulaarilausekkeiden käyttö"
simple_title:         "Regulaarilausekkeiden käyttö"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi Regular Expressioneita Kannattaa Käyttää?

Regular expressioneilla eli säännöllisillä lausekkeilla on monia käyttötarkoituksia ohjelmoinnissa, mutta ne ovat erityisen hyödyllisiä tekstin käsittelyssä. Ne mahdollistavat tehokkaan ja monipuolisen tavan löytää, muokata ja etsiä tekstistä tiettyjä merkkijonoja.

## Miten Käyttää Regular Expressioneita TypeScriptissä?

Regular expressioneita voi käyttää TypeScriptissä sisäänrakennetun RegExp-luokan avulla. Tämä luokka tarjoaa kattavan valikoiman metodeja ja ominaisuuksia, jotka mahdollistavat erilaisten säännöllisten lausekkeiden käytön. Alla on esimerkki siitä, miten voit etsiä kaikki puhelinnumerot tekstistä ja tulostaa ne konsoliin:

```TypeScript
// Alustetaan regular expression puhelinnumeroiden etsimiseen
let regex = new RegExp("[0-9]{3}-[0-9]{3}-[0-9]{4}");

// Esimerkkiteksti, josta etsitään puhelinnumerot
let teksti = "Mun puhelinnumero on 555-123-4567 ja toinen numero on 123-456-7890";

// Etsitään ja tulostetaan löydetyt puhelinnumerot
console.log(teksti.match(regex));
```

Tämä koodi tuottaa seuraavan tulosteen:

```
['555-123-4567', '123-456-7890']
```

## Syvempi Sukellus Regular Expressioneihin

Regular expressionit koostuvat erilaisista merkeistä ja erityisistä komentoketjuista, joita käytetään tietynlaisen merkkijonon löytämiseen. Niitä voi käyttää esimerkiksi haku- ja korvausoperaatioissa, jolloin ne mahdollistavat tehokkaan ja tarkan tekstin käsittelyn.

Regular expressioneilla on myös erilaisia metakaraktereita, jotka edustavat erilaisia merkkejä tai merkkijonoja. Esimerkiksi piste-merkki (.) edustaa yhtä merkkiä ja tähti (*) edustaa yhtä tai useampaa esiintymää edellisestä merkistä.

Tämän lisäksi säännöllisissä lausekkeissa voi olla myös erilaisia määritteitä, kuten `{n}`, jossa n määrittää tietyn merkin tai merkkijonon toistojen määrän. Esimerkiksi `{3}` tarkoittaa, että etsitään kolme peräkkäistä esiintymää edellisestä merkistä.

Jos haluat syventyä tarkemmin regular expressioneihin, suosittelemme lukemaan lisää TypeScriptin [RegExp-luokasta](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/RegExp) ja [säännöllisistä lausekkeista yleisesti](https://fi.wikipedia.org/wiki/S%C3%A4%C3%A4nn%C3%B6llinen_lauseke).

## Katso Myös
- [TypeScriptin virallinen dokumentaatio](https://www.typescriptlang.org/docs/)
- [W3Schools Regular Expression Tutorial](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
- [Regular Expressions Cheat Sheet](https://cheatography.com/davechild/cheat-sheets/regular-expressions/)