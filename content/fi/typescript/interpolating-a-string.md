---
title:                "Merkkijonon interpolointi"
html_title:           "TypeScript: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Stringien interpolointi on tekniikka, jossa ohjelmoijat sisällyttävät muuttujien arvoja osaksi merkkijonoja. Tämä tekee koodista luettavampaa ja vähentää tarvetta yhdistellä merkkijonoja "+" -merkeillä.

Usein tarvitsemme merkkijonoja, jotka sisältävät muuttuvia arvoja, kuten käyttäjän syötteitä tai tietokannasta haettuja tietoja. Stringien interpoloiminen helpottaa näiden muuttujien lisäämistä merkkijonoihin ilman monimutkaisia operaatioita.

## Miten?
Ts._stringin_tehtävaisiin_rajoittaminen=function(kohde) {
  palauta tsString + kohde + "tehtäväsi on rajallinen!";
}

tsString = "Tervetuloa";
tehtävä = "kirjoittaa";

Ts.tee_stringi(tsString,tehtävä);

// Tulostaa "Tervetuloa kirjoittaa tehtäväsi on rajallinen!"

## Syvempi sukellus
Stringien interpolointia käytettiin alun perin C-kielen sprintf-funktiolla, mutta nykyään monet ohjelmointikielet, kuten TypeScript, tarjoavat sisäänrakennetun tavan interpoloida merkkijonoja.

Vaihtoehtoisesti voimme käyttää myös merkkijonojen muotoilua, jossa käytetään %-merkkejä merkitsemään sijoituskohtia muuttujille merkkijonossa. Tämä vaihtoehto voi kuitenkin olla haastavampi ylläpidettävyyden kannalta, koska muuttujat tulee seurata tarkasti ja niiden järjestys merkkijonossa on tärkeä.

Stringien interpolointi TypeScriptissä käyttää välimerkkiä $ avainarvojen edessä osoittamaan, että kyseessä on muuttuja ja ei osa merkkijonoa. Tämä tekee koodista selkeämpää ja vähentää mahdollisuutta virheille.

## Katso myös
[TypeScriptin dokumentaatio stringien interpoloinnista] (https://www.typescriptlang.org/docs/handbook/basic-types.html#string-interpolation)