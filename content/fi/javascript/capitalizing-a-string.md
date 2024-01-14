---
title:    "Javascript: Merkkijonon suurkirjainten muuttaminen"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Miksi

Oletko koskaan miettinyt, miksi ja milloin pitäisi käyttää tekstin muotoilua ohjelmointikielessä? Yksi yleinen syy voi olla esimerkiksi, kun halutaan muuttaa tekstin ulkoasua tai esittää se selkeämmin lukijalle. Tässä blogikirjoituksessa keskitymme erityisesti tekstin muutokseen, eli miten voit muuttaa tekstin kirjainten kokoa ohjelmointikielessä.

## Kuinka

JavaScript-pohjassa on useita tapoja muuttaa tekstin kirjainten kokoa, mutta tässä keskitymme yhteen yleisimmistä – suurien kirjainten muuttamiseen pieniksi ja päinvastoin. Tässä esimerkissä käytämme `toUpperCase()` ja `toLowerCase()` -funktioita, jotka ovat käytännössä vastakkaisia toisilleen. Katso alla olevaa koodia ja sen tulosteita:

```Javascript
var teksti = "tervetuloa suomeen!"
console.log(teksti.toUpperCase());
// TULOSTUS: TERVETULOA SUOMEEN!

console.log(teksti.toLowerCase());
//TULOSTUS: tervetuloa suomeen!
```

Tässä yksinkertaisessa esimerkissä muutamme tekstissä kaikki kirjaimet joko isoiksi tai pieniksi. Käytännössä voit käyttää näitä funktioita missä tahansa tilanteessa, jossa haluat muuttaa tekstin kirjainten kokoa.

## Syvempi sukellus

Entä jos haluat muuttaa vain tietyt kirjaimet pieniksi tai isoiksi? Tällöin voit käyttää `charAt()` -funktiota, joka ottaa parametrina kirjaimen sijainnin tekstissä ja palauttaa sen arvon. Yhdistämällä tämän funktioon `toUpperCase()` tai `toLowerCase()` voit muuttaa vain haluamasi kirjaimet.

```Javascript
var teksti = "tervetuloa suomeen!"
var uusiTeksti = "";

for (var i = 0; i < teksti.length; i++){
    if (i % 2 == 0){
        uusiTeksti += teksti.charAt(i).toUpperCase();
    } else {
        uusiTeksti += teksti.charAt(i).toLowerCase();
    }
}
console.log(uusiTeksti);
// TULOSTUS: TeRvEtUlOa SuOmEeN!
```

Tässä esimerkissä käytämme for-loopia ja `charAt()` -funktiota luodaksemme uuden tekstin, jossa joka toinen kirjain on suuri ja joka toinen pieni. Tämä osoittaa, kuinka voit käyttää tekstin muotoilua joustavasti riippuen omista tarpeistasi.

## Katso myös

- [MDN web docs: String prototyypit](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String)
- [W3Schools: JavaScript string methods](https://www.w3schools.com/js/js_string_methods.asp)
- [FreeCodeCamp: How to convert strings to lowercase, uppercase, or title case in JavaScript](https://www.freecodecamp.org/news/how-to-convert-strings-to-lowercase-uppercase-or-title-case-in-javascript/)