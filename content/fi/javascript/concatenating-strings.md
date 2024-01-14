---
title:                "Javascript: Merkkijonon yhdistäminen"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

JavaScript-ohjelmoijat käyttävät usein merkkijonojen yhdistämistä, jotta he voivat luoda dynaamisia ja muuttuvia tekstipalasia. Tämä on erityisen hyödyllistä, kun halutaan luoda viestejä tai tekstejä, jotka sisältävät tietoja, jotka muuttuvat käyttäjän toimintojen tai tietojen perusteella.

## Miten

Seuraavaksi annan muutaman esimerkin, jotka voit toistaa kokeillaksesi merkkijonojen yhdistämistä JavaScriptillä. Koodiesimerkkien tulokset on näytetty kommenttien vieressä.

```Javascript
var nimi = "Maria";
var ika = 28;
console.log("Tervetuloa " + nimi + "! Olet " + ika + " vuotta vanha.");
// Tuloste: Tervetuloa Maria! Olet 28 vuotta vanha.
```

```Javascript
var tuote = "t-paita";
var maara = 3;
console.log("Ostit " + maara + " " + tuote + "a.");
// Tuloste: Ostit 3 t-paitaa.
```

```Javascript
var aika = "10"; // huomaa, että tämä on merkkijono eikä numero
console.log("Sivustolla on käynyt " + aika + " käyttäjää tänään.");
// Tuloste: Sivustolla on käynyt 10 käyttäjää tänään.
```

## Syvällisempi tarkastelu

Merkkijonojen yhdistäminen JavaScriptillä käyttää plus-merkkiä (+) yhdistämään merkkijonoja toisiinsa. On tärkeää huomata, että tulosteen tyyppi riippuu ensimmäisestä operandista eli siitä, mikä merkkijono toimii ensimmäisenä. Jos ensimmäinen operandi on numero, JS muuttaa sen automaattisesti merkkijonoksi ja lisää sen toisen operandin kanssa.

Lisäksi on mahdollista käyttää muita menetelmiä, kuten `concat()` ja `join()` yhdistämiseen, mutta tässä artikkelissa keskitymme plus-merkin käyttöön.

## Katso myös

- [MDN Web Docs: Yhdistäminen](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Operators/Concatenation) 
- [W3Schools: Merkkijonojen yhdistäminen](https://www.w3schools.com/js/js_string_concat.asp)