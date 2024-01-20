---
title:                "Merkkien poistaminen vastaavalla mallilla"
html_title:           "Arduino: Merkkien poistaminen vastaavalla mallilla"
simple_title:         "Merkkien poistaminen vastaavalla mallilla"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Poistaminen Merkkijonoista Täsmällisten Merkkien Perusteella JavaScriptillä 

## Mikä & Miksi?

Merkkijonon muokkaus hahmojen poistamisen avulla on yksi tapa manipuloida tietoja JavaScriptillä. Tämä on tarpeen, kun esimerkiksi halutaan siivota käyttäjän syötteitä tai poistaa tarpeettomia merkkejä tiedoista.

## Näin se toimii:

JavaScriptissä merkkien poistaminen tehdään `.replace()` metodilla yhdessä säännöllisten lausekkeiden kanssa.

```javascript
let str = "Tervetuloa, ohjelmointiin";
let newStr = str.replace(/o/g, "");
console.log(newStr);
```

Tämä poistaa kaikki 'o' kirjaimet merkkijonosta. Tulostuksesi pitäisi näyttää seuraavalta:

```output
"Tervetul, hjelmintiin"
```

## Deep Dive

### Historiallinen Konteksti
JavaScriptin `.replace()` metodia on käytetty merkkijonon manipuloinnissa jo pitkään. Säännölliset lausekkeet lisättiin kieleen vasta myöhemmin, tehostamaan merkkijonojen käsittelyä.

### Vaihtoehdot
Voit myös käyttää `.split()` ja `.join()` metodeja, jos haluat poistaa tietyn merkin:

```javascript
let str = "Tervetuloa, ohjelmointiin";
let newStr = str.split('o').join('');
console.log(newStr);
```

### Toteutuksen yksityiskohdat
Säännölliset lausekkeet tekevät `.replace()` metodista voimakkaamman. Esimerkiksi `/o/g` säännöllinen lauseke tarkoittaa "kaikista 'o' merkeistä merkkijonossa". 'g' on ns. 'global' lippu - se tarkoittaa, että kaikki kirjaimet korvataan, ei vain ensimmäinen.

## Katso myös

- [Mozilla Developer Network: String.prototype.replace()](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Mozilla Developer Network: Säännölliset lausekkeet](https://developer.mozilla.org/fi/docs/Web/JavaScript/Guide/Regular_Expressions)
- [JavaScript Tutorial: Splitting and Joining Strings](https://www.javascripttutorial.net/javascript-string-split/)