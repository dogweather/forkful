---
title:                "Javascript: Alimerkkijonojen erottaminen"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Joskus käyttäjät tarvitsevat tietyn osan merkkijonosta sen sijaan, että käyttäisivät koko merkkijonoa. Substringin eli osakerroksen eristäminen on siis erittäin hyödyllistä monissa ohjelmointitilanteissa.

## Miten

Alla on esimerkki, joka näyttää, miten merkkijonon osakerros voidaan eristää käyttämällä Javascriptin substring-funktiota.

```Javascript
let teksti = "Tämä on esimerkki merkkijonosta!";
let sub = teksti.substring(8, 15);
console.log(sub);
```

Tulostus: "esimerkki"

In yllä olevassa esimerkissä substring-funktio ottaa kaksi parametria: alkuindeksin ja loppuindeksin. Alkuindeksi määrittelee, mistä merkkijonon kohdasta osakerros alkaa, ja loppuindeksi määrittelee, mihin kohtaan se päättyy. Loppuindeksi ei sisälly itse osakerrokseen.

Voit myös antaa vain yhden parametrin, jolloin substring ottaa alkuindeksin ja päättyy merkkijonon loppuun:

```Javascript
let teksti = "Tämä on esimerkki merkkijonosta!";
let sub = teksti.substring(12);
console.log(sub);
```

Tulostus: "esimerkki merkkijonosta!"

Substringin lisäksi Javascriptissa on myös slice-funktio, jota voidaan käyttää samalla tavalla. Ainoa ero on, että slice-haarasleikkaus ei hyväksy negatiivisia indeksejä, kun taas substring voi.

## Syvemmälle

Merkkijonon osakerroksen erottaminen on tärkeä osa Javascript-ohjelmointia. Se voi auttaa käyttäjiä hallitsemaan ja manipuloimaan dataa helposti ja tehokkaasti.

On kuitenkin tärkeää muistaa, että merkkijonon indeksointi alkaa aina nollasta. Tämä tarkoittaa sitä, että ensimmäinen merkki merkkijonossa on indeksissä 0, toinen indeksissä 1 ja niin edelleen. Myös päättyvä indeksi ei sisälly itse osakerrokseen, mikä voi joskus aiheuttaa virheitä, jos se unohdetaan.

Lisäksi merkkijonojen käsitteleminen voi olla hieman erilaista eri ohjelmointikielissä, joten on tärkeää tarkistaa dokumentaatio ennen substringin tai slice-haarasleikkauksen käyttöä.

## Katso myös

- [MDN: String.substring](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN: String.slice](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [W3Schools: Javascript String Methods](https://www.w3schools.com/js/js_string_methods.asp)