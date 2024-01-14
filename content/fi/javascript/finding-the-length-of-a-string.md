---
title:                "Javascript: Merkkijonon pituuden löytäminen"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Miksi etsiä merkkijonon pituus? 

Merkkijonon pituuden löytäminen on yksi yleisimmin käytetyistä toiminnoista ohjelmoinnissa. Se on hyödyllistä esimerkiksi kun haluat tarkistaa, että käyttäjän antama syöte on tietyn mittainen tai kun haluat käsitellä tietystä pituudesta olevia merkkijonoja. Tässä blogikirjoituksessa opimme, kuinka löytää merkkijonon pituus ja mitä muuta siihen liittyy.

## Kuinka tehdä

Ensinnäkin, meidän täytyy ymmärtää, että merkkijonot ovat JavaScriptissä luokkaa **String** ja niillä on oma sisäinen ominaisuus **length**, joka kertoo merkkijonon pituuden. Voimme käyttää tätä ominaisuutta seuraavasti:

```javascript
let merkkijono = "Tervetuloa lukemaan blogia!";
console.log(merkkijono.length); // Tulostaa merkkijonon pituuden, tässä tapauksessa 28
```

Tässä esimerkissä luomme muuttujan **merkkijono** ja tallennamme siihen haluamamme merkkijonon. Sitten käytämme sisäistä **length** ominaisuutta ja tulostamme sen konsoliin.

Voimme myös käyttää **length** ominaisuutta suoraan ilman muuttujaa:

```javascript
console.log("Moi".length); // Tulostaa merkkijonon pituuden, tässä tapauksessa 3
```

## Syväsukellus

Merkkijonon pituuden löytäminen voi vaikuttaa yksinkertaiselta, mutta se voi myös aiheuttaa joitakin yllätyksiä. Esimerkiksi tyhjä merkkijono näyttää olevan pituudeltaan 0, mutta se ei olekaan niin yksinkertainen. Tässä esimerkki:

```javascript
console.log("".length); // Tulostaa 0
```

Näyttääkö siltä, että pituus on nolla? Todellisuudessa kyseessä on yksi tyhjä merkki, joka ei näy. Tämä tarkoittaa, että tyhjän merkkijonon pituus ei ole oikeasti nolla, vaan yksi.

Entä mitä tapahtuu, kun annamme **length** ominaisuuden muille datatyypeille, kuten numerolle? Yllätykseksemme, se näyttää myös toimivan:

```javascript
let numero = 5687;
console.log(numero.length); // Tulostaa 4
```

Tämä johtuu siitä, että JavaScript muuttaa numeron automaattisesti merkkijonoksi, jolloin sen pituus voidaan laskea. Tämä voi aiheuttaa ongelmia, jos haluat tarkistaa käyttäjän antaman syötteen pituuden, sillä käyttäjä voi antaa vahingossa numeron ajattelematta, että sen pituus lasketaan myös. Tässä tapauksessa voit käyttää **typeof** operaattoria tarkistaaksesi, onko syöte merkkijono vai ei.

## Katso myös 

- [MDN - String.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length) 
- [W3Schools - JavaScript String length Property](https://www.w3schools.com/jsref/jsref_length_string.asp) 
- [tutorialspoint - JavaScript String Length Property](https://www.tutorialspoint.com/javascript/js_string_length.htm)