---
title:                "Päivämäärän jäsentäminen merkkijonosta"
html_title:           "Javascript: Päivämäärän jäsentäminen merkkijonosta"
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Merkkijonosta päivämäärän jäsennys on prosessi, jossa analyyttisesti muutetaan tekstiä päivämääräobjektiin Javascript-kieltä varten. Ohjelmoijat tekevät sen, jotta he pystyvät manipuloimaan aikatietoja ja suorittamaan toimintoja, kuten aikavertailuja ja aikavälien laskemista.

## Näin se toimii:
Ohessa on joitakin esimerkkejä siitä, miten päivämäärä voidaan jäsentää merkkijonosta Javascriptin avulla.
```Javascript
// Tavanomainen tapa on käyttää sisäänrakennettua Date constructoria
let d = new Date("2021-02-18");
console.log(d.toString());  // Outputs: "Thu Feb 18 2021 02:00:00 GMT+0200 (Eastern European Standard Time)"

// Lisäksi voit käyttää Date.parse-metodia, joka palauttaa ajan millisekunneissa Unix Epochista lähtien
let ms = Date.parse("2021-02-18");
console.log(ms);  // Outputs: 1613587200000
```

## Syväsukellus
Jäsennettäessä päivämäärä merkkijonosta on tärkeää ymmärtää sen historiallinen konteksti Javascriptin ohjelmointikielessä. Javascript Java-kielessä Date-olion ensimmäinen versio käytettiin tähän tarkoitukseen 90-luvun puolivälissä. Jotkut näistä varhaisista keinoista ovat nyt vanhentuneita, joten on tärkeää pysyä ajan tasalla uusimmista standardeista.

Vaihtoehtoiset menetelmät merkkijonosta päivämäärän jäsentämiseksi sisältävät kolmannen osapuolen kirjastojen, kuten moment.js tai day.js, käytön. Nämä kirjastot tarjoavat useita työkaluja ja ominaisuuksia päivämäärien ja ajan käsittelyyn.

Tehtäessä näitä merkkijonosta päivämäärän jäsentämisiä on tärkeää varmistaa tiedon oikeellisuus ja käsittelyn avaaminen. Javascriptin sisäänrakennettu Date-olio ja metodit tarjoavat useita eri tapoja tehdä tätä, mutta ne voivat tuottaa erilaisia tuloksia eri selaimissa ja alustoilla.

## Katso myös:
* [Mozilla Developer Network: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
* [Moment.js](https://momentjs.com/)
* [Day.js](https://day.js.org/)
* [ECMAScript Specification](https://www.ecma-international.org/ecma-262/12.0/index.html#sec-date-objects)