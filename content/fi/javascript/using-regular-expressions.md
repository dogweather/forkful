---
title:    "Javascript: Säännöllisten lausekkeiden käyttö"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita?

Säännölliset lausekkeet ovat hyödyllisiä ohjelmoinnissa, jos haluat tarkistaa tai muokata tekstiä tietyllä tavalla. Ne ovat erityisen hyödyllisiä, kun haluat löytää tai korvata tietynlaisia merkkijonoja, kuten puhelinnumeroita tai sähköpostiosoitteita.

## Näin käytät säännöllisiä lausekkeita

Säännöllisten lausekkeiden käyttö Javascriptissa tapahtuu RegExp-olion avulla. Voit luoda uuden säännöllisen lausekkeen kirjoittamalla sen sisältämän merkkijonon välissä etukenoinen kauttalainen (\) ja käyttämällä sille haluamiasi Englannin aakkosia. Esimerkiksi, haluaisit löytää kaikki "koira" -sanalla alkavat lauseet, voit käyttää säännöllistä lauseketta `/^koira/`.

```Javascript
// Luodaan säännöllinen lauseke
var regex = /^koira/;

// Testataan lauseke ja tallennetaan tulos muuttujaan
var result = regex.text("Koirat haukkuvat kovaa");
console.log(result); // output: true
```

Tässä esimerkissä käytämme `text()`-metodia säännöllisen lausekkeen tarkistamiseen. Jos haluamme korvata löydetyt merkkijonot uudella tekstillä, voimme käyttää `replace()`-metodia. Voimme myös käyttää luomiamme ryhmiä (`()`-merkit) säännöllisessä lausekkeessa, ja käyttää niitä myöhemmin korvaamisessa.

```Javascript
// Esimerkki ryhmien käytöstä
var regex = /^(koira)o/;

// Korvataan löydetyt merkkijonot ryhmän ensimmäisellä sanalla
var result = "Koirat ja kissat".replace(regex, "$1ira"); 
console.log(result); // output: Koira ja kissat
```

## Syvempi sukellus säännöllisiin lausekkeisiin

Säännöllisissä lausekkeissa on monia muita hyödyllisiä ominaisuuksia, kuten metakaraktereita ja kvantorimerkkejä. Metakaraktereilla voidaan esimerkiksi tarkistaa tietynlaisia merkkejä, kuten numerot (`\d`) tai välilyönnit (`\s`). Kvantorimerkkien avulla voidaan määrittää, kuinka monta kertaa jokin merkki tai ryhmä esiintyy, kuten `+` (yksi tai useampi) tai `?` (nolla tai yksi).

Jos haluat oppia lisää säännöllisistä lausekkeista ja niiden käytöstä Javascriptissa, suosittelen tutustumaan MDN:n sivuille tai katsomaan opetusvideoita YouTubesta.

## Katso myös

- [MDN: Säännölliset lausekkeet](https://developer.mozilla.org/fi/docs/Web/JavaScript/Guide/Regular_Expressions)
- [W3schools: Javascript Regular Expressions](https://www.w3schools.com/js/js_regexp.asp)
- [JavaScript.info: Säännölliset lausekkeet](https://javascript.info/regexp-introduction)