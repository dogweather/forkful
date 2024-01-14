---
title:                "Javascript: Näytön pituuden löytäminen"
simple_title:         "Näytön pituuden löytäminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

Miksi: Miksi joku haluaisi selvittää merkkijonon pituuden?

Merkkijonot ovat tärkeitä tietorakenteita, jotka sisältävät tekstiä tai merkkejä. Näitä merkkijonoja käytetään laajasti ohjelmoinnissa, joten on tärkeää ymmärtää niiden ominaisuudet, kuten pituus. Pituuden selvittäminen auttaa meitä ymmärtämään ja manipuloimaan merkkijonoja paremmin ohjelmoinnissa.

Miten: Koodiesimerkkejä ja tulosteita.

Jos haluat selvittää merkkijonon pituuden Javascriptissä, voit käyttää *length* -ominaisuutta. Tämä ominaisuus antaa meille merkkijonon pituuden numeromuodossa.

```javascript
let sana = "Tervetuloa!";
console.log(sana.length); //tulostaa 11
```

Huomaa, että tämä ominaisuus ei laske pelkästään kirjaimia, vaan myös välimerkkejä ja välilyöntejä. Se myös huomioi erikoismerkit, kuten ääkköset.

```javascript
let lause = "Olen ohjelmoija 🚀";
console.log(lause.length); //tulostaa 18
```

Voit myös käyttää *length* -ominaisuutta yhdistettynä *toString* -metodiin, jos haluat muuttaa pituuden numeron merkkijonoksi.

```javascript
let nimi = "Maria";
console.log("Olen " + nimi.length.toString() + " kirjainta pitkä"); //tulostaa "Olen 5 kirjainta pitkä"
```

Syötteestä riippuen *length* -ominaisuuden käyttö voi vaihdella hieman, mutta sen peruskäyttö on sama.

Deep Dive: Syvemmälle merkkijonojen pituuden selvittämisen taustatietoihin

Merkkijonot ovat Javascriptissä merkkijonoina tallennettuja merkkijonoja. Jokaisella merkkijonolla on oma pituus ja siihen liittyvä *length* -ominaisuus. Tämä ominaisuus on peräisin *String.prototype* -objektilta, joka antaa meille pääsyn kaikkiin merkkijonomenetelmiin.

Merkkijonon pituus mitataan merkkien määränä. Tämä tarkoittaa, että *length* -ominaisuus laskee jokaisen merkin, mukaan lukien erikoismerkit ja välilyönnit. Tämä on tärkeää ottaa huomioon, kun käytät sitä ohjelmoinnissa.

See Also (Katso myös):

- https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String/length
- https://www.w3schools.c