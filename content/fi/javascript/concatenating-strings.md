---
title:    "Javascript: Yhdistämisen merkkijonot"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Miksi

Usein kun ohjelmoidaan, tarvitaan tekstiketjuja tai -merkkijonoja. Näiden tekstiketjujen yhdistäminen tai "concatenating" on yksi tärkeä osa JavaScript-ohjelmointia. Se auttaa luomaan monipuolisia ja dynaamisia tekstejä, jotka voivat vaihdella sen mukaan, mitä tietoja ohjelma saa.

## Kuinka tehdä

Kun haluat yhdistää kaksi tai useampia merkkijonoja yhdeksi, voit käyttää JavaScriptin perusmuotoa:

```Javascript 
var string1 = "Hei";
var string2 = "maailma";

var result = string1 + " " + string2;

console.log(result); // Output: "Hei maailma"
```

Voit myös yhdistää muuttujia ja muita merkkijonoja samalla tavalla. Voit myös käyttää erilaisia operaattoreita, kuten `+=` ja `concat()`.

## Syventävä tieto

On tärkeää huomata, että kun yhdistät merkkijonoja JavaScriptissä, lopputulos on uusi merkkijono. Vanhat merkkijonot pysyvät ennallaan. Tämä johtuu siitä, että merkkijonot ovat ei-muuttuvia tai "immutable" JavaScriptissä. Siksi on tärkeää tallentaa yhdistetty merkkijono uuteen muuttujaan, jos haluat käyttää sitä myöhemmin ohjelmassa.

Jokaisella merkkijonolla on myös oma pituutensa, joka voidaan selvittää `length`-ominaisuuden avulla:

```Javascript 
var string = "Tämä on tekstiketju";

console.log(string.length); // Output: 20
```

Lisäksi voit käyttää `substring()`-metodia palauttamaan osan merkkijonosta:

```Javascript 
var string = "Tämä on tekstiketju";

console.log(string.substring(5,8)); // Output: " on"
```

## Katso myös

- [MDN web docs: String Concatenation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/String_Concatenation)
- [W3Schools: JavaScript Strings](https://www.w3schools.com/js/js_strings.asp)
- [Codecademy: Concatenation](https://www.codecademy.com/courses/introduction-to-javascript/lessons/introduction-to-javascript/exercises/concatenation)