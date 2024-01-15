---
title:                "Jonojen yhdistäminen"
html_title:           "TypeScript: Jonojen yhdistäminen"
simple_title:         "Jonojen yhdistäminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Ensimmäinen askel oppiessa TypeScriptia on ymmärtää datan tyypin merkitys ja muotoilu ohjelmissa. Yksi tärkeä käsite, joka auttaa ohjelmoijia käsittelemään datan muotoilua, on merkkijonojen yhdistäminen. Tässä artikkelissa tarkastellaan, miksi ja miten merkkijonoja yhdistetään TypeScriptissä.

## Miten

Merkkijonojen yhdistäminen (tai concatenation) on prosessi, jossa kaksi tai useampia merkkijonoja yhdistetään yhdeksi. Tämä tehdään yleensä käyttämällä + operaattoria, joka yhdistää kaksi merkkijonoa ja palauttaa uuden yhdistetyn merkkijonon. Tässä on yksinkertainen esimerkki:

```TypeScript
let firstName: string = "Matti";
let lastName: string = "Meikäläinen";
let fullName: string = firstName + " " + lastName;

console.log(fullName); // Tulostaa "Matti Meikäläinen"
```

Jos haluat sisällyttää muuttujan arvon osaksi merkkijonoa, voit käyttää myös backtick (`) ja ${} syntaksia, joka mahdollistaa merkkijonon sisällä muuttujien lisäämisen. Tässä on esimerkki:

```TypeScript
let city: string = "Helsinki";
let landmark: string = "Tuomiokirkko";
let description: string = `Seuraava kohde matkallasi ${city}  on ${landmark}.`;

console.log(description); // Tulostaa "Seuraava kohde matkallasi Helsinki on Tuomiokirkko."
```

## Syvällinen sukellus

Merkkijonojen yhdistämisellä on useita käyttötapoja TypeScriptissä. Yksi yleisimmistä on merkkijonojen rakentaminen käyttäjän syöttämien tietojen perusteella. Esimerkiksi lomakkeessa, jossa pyydetään käyttäjän nimeä, voit yhdistää etu- ja sukunimen ja tulostaa "Tervetuloa, [nimi]" -viestin.

Merkkijonojen yhdistäminen on myös hyödyllistä kun halutaan luoda dynaamisia viestejä ja ilmoituksia. Esimerkiksi jos sinulla on verkkokauppa, joka tarjoaa alennuskoodin, voit yhdistää alennuskoodin merkkijonoon ja näyttää sen käyttäjälle ostoskorissa.

## Katso myös

- [TypeScript-luokat] (https://www.typescriptlang.org/docs/handbook/classes.html)
- [Tyyppien vaikutukset ohjelmointiin TypeScriptissä] (https://medium.com/@sashko6613/how-types-affect-programming-in- typescript-90a2b0dbe452)