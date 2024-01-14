---
title:                "TypeScript: Mallia vastaavien merkkien poistaminen"
simple_title:         "Mallia vastaavien merkkien poistaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmointitehtävän yhteydessä saattaa olla tarve poistaa merkkejä, jotka vastaavat tiettyä kaavaa. Tämä voi olla hyödyllistä esimerkiksi silloin, kun halutaan puhdistaa tietoa tai hallita tietokantoja.

## Kuinka tehdä

Seuraavassa esittelemme kaksi eri tapaa poistaa merkkejä, jotka vastaavat tiettyä kaavaa TypeScript-ohjelmoinnissa.

### Metodi 1: String-metodi `replace()`

```TypeScript
// Alustetaan merkkijono, josta halutaan poistaa merkkejä
const string = "Tämä on esimerkkilause.";
// Alustetaan kaava, johon perustuen merkkejä poistetaan
const pattern = /ä|ö|y/g;
// Käytetään String-metodia replace() ja annetaan kaava parametriksi
const newString = string.replace(pattern, "");
// Tulostetaan uusi merkkijono konsoliin
console.log(newString); // Tulostaa: Tm on esimerkkilause.
```

Kuten nähdään, `replace()`-metodi ottaa parametrina vastaan kaavan, johon perustuen merkkejä poistetaan. Tässä tapauksessa kaava `/ä|ö|y/g` tarkoittaa, että kaikki esiintymät merkeistä "ä", "ö" ja "y" poistetaan.

### Metodi 2: String-metodi `splice()`

```TypeScript
// Alustetaan merkkijono, josta halutaan poistaa merkkejä
const string = "Tämä on esimerkkilause.";
// Alustetaan kaava, johon perustuen merkit poistetaan
const pattern = /[A-ZÄÖÅÜ][a-zäöåü]/g;
// Käytetään String-metodia split() ja annetaan kaava parametriksi
const array = string.split(pattern);
// Käytetään Array-metodia join() ja annetaan tyhjä merkkijono parametriksi
const newString = array.join("");
// Tulostetaan uusi merkkijono konsoliin
console.log(newString); // Tulostaa: n on rekkils..
```

Tässä esimerkissä käytämme `split()`-metodia, joka jakaa merkkijonon osiin annetun kaavan perusteella. Parametrina annettava kaava `/[A-ZÄÖÅÜ][a-zäöåü]/g` tarkoittaa, että poistetaan yhden ison kirjaimen ja yhden pienen kirjaimen yhdistelmät. Tämän jälkeen käytämme `join()`-metodia, joka yhdistää osat takaisin yhdeksi merkkijonoksi. Lopputuloksena saamme merkkijonon, josta poistettu kaikki isot ja pienet kirjaimet välilyönteineen.

## Syvällisempi tarkastelu

Molemmat esitellyt metodit toimivat hyvin merkkien poistamiseen kaavan perusteella. On kuitenkin tärkeää huomata, että `replace()`-metodi vaikuttaa alkuperäiseen merkkijonoon, kun taas `split()` ja `join()` eivät vaikuta alkuperäiseen merkkijonoon vaan luovat uuden taulukon. Lisäksi kaavan käytössä on huomioitava, että se saattaa muuttua riippuen minkä tyyppisiä merkkejä halutaan poistaa ja millaisia sääntöjä on noudatettava.

## Katso myös

- [TypeScript String Methods](https://www.w3schools.com/js/js_string_methods.asp)
- [TypeScript Regular Expressions](https://www.w3schools.com/js/js_regexp.asp)