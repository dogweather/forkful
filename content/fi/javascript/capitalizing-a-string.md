---
title:                "Javascript: Merkkijonon isot kirjaimet"
simple_title:         "Merkkijonon isot kirjaimet"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Ääkkösten käsittely on tärkeä osa ohjelmointia, erityisesti suomenkielistä koodia kirjoitettaessa. Pienikin virhe ääkkösten käsittelyssä voi aiheuttaa ohjelman toimimattomuuden, joten on tärkeää ymmärtää miten niitä käsitellään oikein. Tässä artikkelissa keskitymme merkkijonon kirjainkoon muuttamiseen Javascriptillä, eli miten muuttaa kaikki kirjaimet isoihin tai pieniin kirjaimiin.

## Miten

Merkkijonon kirjainkoon muuttaminen Javascriptillä on helppoa käyttämällä sisäänrakennettuja funktioita, kuten `.toLowerCase()` ja `.toUpperCase()`. Nämä funktiot palauttavat uuden merkkijonon, jossa kaikki kirjaimet ovat joko pieniä tai isoja.

Esimerkki pienennetyn merkkijonon luomisesta:

```Javascript
let sana = "KISSA";
let pienetKirjaimet = sana.toLowerCase();

console.log(pienetKirjaimet); // tulostaa "kissa"
```

Esimerkki isonnetun merkkijonon luomisesta:

```Javascript
let sana = "koira";
let isotKirjaimet = sana.toUpperCase();

console.log(isotKirjaimet); // tulostaa "KOIRA"
```

Merkkijonon kirjainkoon muuttamisen lisäksi on myös mahdollista muuttaa vain ensimmäinen kirjain isoksi tai pieneksi käyttämällä `.charAt()` ja `.toUppercase()` tai `.toLowerCase()` yhdessä.

Esimerkki ensimmäisen kirjaimen muuttamisesta isoksi:

```Javascript
let sana = "hey";
let muokattuSana = sana.charAt(0).toUpperCase() + sana.slice(1);

console.log(muokattuSana); // tulostaa "Hey"
```

## Syväsukellus

Javascriptillä on myös muita keinoja merkkijonon kirjainkoon muuttamiselle, kuten käyttämällä `.replace()` tai `.substr()` funktioita. `.replace()` mahdollistaa tietyn kirjaimen tai merkkijonon korvaamisen uudella, ja `.substr()` palauttaa merkkijonon halutusta kohdasta alkaen määritellyn pituuden verran.

Esimerkki `.replace()` käytöstä:

```Javascript
let sana = "mittari";
let vaihdettuSana = sana.replace('i', 'e');

console.log(vaihdettuSana); // tulostaa "mettari"
```

Esimerkki `.substr()` käytöstä:

```Javascript
let sana = "kirja";
let osasana = sana.substr(1, 3);

console.log(osasana); // tulostaa "irj"
```

On myös mahdollista luoda oma funktio, joka muuttaa merkkijonon kirjainkoon halutulla tavalla. Tässä esimerkki funktiosta, joka muuttaa jokaisen toisen kirjaimen isoksi:

```Javascript
function jokaToinenIsoksi(sana) {
    let uusiSana = "";

    for (let i = 0; i < sana.length; i++) {
        if (i % 2 === 0) {
            uusiSana += sana.charAt(i).toUpperCase();
        } else {
            uusiSana += sana.charAt(i);
        }
    }

    return uusiSana;
}

let alkuperainenSana = "auto";
let muokattuSana = jokaToinenIsoksi(alkuperainenSana);

console.log(muokattuSana); // tulostaa "aUto"
```

## Katso myös

- [MDN: .toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [MDN: .toUpperCase()](https://developer.mozilla.org