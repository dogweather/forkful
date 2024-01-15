---
title:                "Säännöllisten ilmaisujen käyttö"
html_title:           "Javascript: Säännöllisten ilmaisujen käyttö"
simple_title:         "Säännöllisten ilmaisujen käyttö"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita?

Säännölliset lausekkeet ovat voimakas työkalu, jota voi käyttää Javascriptissä tiedonkäsittelyssä. Ne tarjoavat yksinkertaisen ja tehokkaan tavan hakea ja manipuloida merkkijonoja. Joten, jos haluat säästää aikaa ja vaivaa tiedonkäsittelyssä, säännölliset lausekkeet ovat loistava apuväline.

## Kuinka käyttää säännöllisiä lausekkeita?

```Javascript
// Luo säännöllinen lauseke, joka etsii sanoja "Javascript" ja "ohjelmointi"
const regex = /Javascript|ohjelmointi/;

// Luo merkkijono, jossa haetaan säännöllisen lausekkeen avulla
const text = "Javascript on loistava ohjelmointikieli.";

// Käytä test-metodia tarkistamaan, löytyykö merkkijonosta haettuja sanoja
console.log(regex.test(text)); // Output: true

// Käytä replace-metodia vaihtaaksesi säännölliseen lausekkeeseen pohjautuvan merkkijonon uuteen
console.log(text.replace(regex, "HTML ja CSS")); // Output: HTML ja CSS on loistava ohjelmointikieli.
```

Säännöllisiä lausekkeita voi käyttää myös monimutkaisempien haun ja manipuloinnin tehtävien suorittamiseen. Voit mm. käyttää määriteltyä säännöllistä lauseketta hakuun, vaihtaa merkkejä ja tarkistaa syötteen oikeellisuuden.

## Syvemmälle säännöllisiin lausekkeisiin

Säännöllisten lausekkeiden käyttö Javascriptissä on todella laaja-alaista. Voit käyttää erilaisia metodeja, kuten test, replace, search ja match, saadaksesi tarkemman käsityksen säännöllisten lausekkeiden toimintaperiaatteista ja mahdollisuuksista.

On myös tärkeää ymmärtää eri merkitykset ja käyttötarkoitukset säännöllisiin lausekkeisiin liittyvistä erikoismerkeistä ja ilmauksista. Voit löytää lisätietoja säännöllisistä lausekkeista esimerkiksi MDN:n (Mozilla Developer Network) sivuilta.

## Katso myös

- [MDN: Säännölliset lausekkeet](https://developer.mozilla.org/fi/docs/Web/JavaScript/Guide/Regular_Expressions)
- [W3Schools: Säännölliset lausekkeet](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
- [JavaScript.info: Säännölliset lausekkeet](https://javascript.info/regular-expressions)