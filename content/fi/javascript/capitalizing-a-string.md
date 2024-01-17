---
title:                "Merkkijonon ensimmäisen kirjaimen muuttaminen isoksi"
html_title:           "Javascript: Merkkijonon ensimmäisen kirjaimen muuttaminen isoksi"
simple_title:         "Merkkijonon ensimmäisen kirjaimen muuttaminen isoksi"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Stringin muuttaminen isoiksi kirjaimiksi tarkoittaa, että jokaisen merkin ensimmäistä kirjoitusasua muutetaan tarpeen mukaan isoksi. Tätä tehdään usein esimerkiksi otsikoissa tai käyttäjänimen syöttökentissä, jotta ne näyttävät selkeämmiltä ja helpommin luettavilta. Ohjelmoijat käyttävät tätä toimintoa myös silloin, kun he haluavat vertailla kahta merkkijonoa, sillä pienten ja isojen kirjainten ero voi vaikuttaa vertailutuloksiin.

## Kuinka:

```javascript
const string = "hei kaikki!";
const capitalizedString = string.toUpperCase();
console.log(capitalizedString); // TULOSTAA: HEI KAIKKI!
```

Tai voit käyttää tätä funktiota stringin sisällä:

```javascript
const string = "hello world!";
console.log(string.toUpperCase()); // TULOSTAA: HELLO WORLD!
```

## Syväsukellus:

Historiallisessa kontekstissa, merkkijonon muuttaminen isoiksi kirjaimiksi oli paljon monimutkaisempaa verrattuna nykyiseen Javascript-vaihtoehtoon. Kehittäjien täytyi käyttää monimutkaisia algoritmeja ja toimintoja, jotta tämä pystyttiin tekemään. Nykyään Javascript tarjoaa helpomman ja nopeamman tavan tehdä tämä muunnos.

Jos haluat muuttaa merkkijonon vain ensimmäisen kirjaimen isoksi ja jättää muut kirjaimet pieniksi, voit käyttää funktiota "slice" yhdistettynä "toUpperCase" funktioon.

Esimerkiksi:

```javascript
const string = "hello world!";
const firstLetter = string.slice(0, 1).toUpperCase();
console.log(firstLetter + string.slice(1)); // TULOSTAA: Hello world! 
```

## Katso myös:

- [Javascript String Methods](https://www.w3schools.com/js/js_string_methods.asp)
- [Slice vs Substring Javascript](https://www.javascripttutorial.net/strings/javascript-string-substring/)
- [Captializing First Letter of a String in JavaScript](https://kodementor.com/captializing-first-letter-of-a-string-in-javascript/)