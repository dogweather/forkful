---
title:                "Javascript: Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

Miksi muuntaa päivämäärä merkkijonoksi?

Päivämäärän muuntaminen merkkijonoksi on tärkeä osa monien ohjelmien toimintaa. Se auttaa meitä esittämään tietoja selkeästi ja ymmärrettävästi käyttäjille sekä hallitsemaan erilaisia aikavyöhykkeitä. Se myös mahdollistaa päivämäärätietojen tallentamisen tietokantaan tai tiedostoon.

Kuinka muuntaa päivämäärä merkkijonoksi?

Päivämäärän muuntaminen merkkijonoksi on helppoa JavaScriptin avulla. Käytä vain "toString ()" -metodia päivämääräolion jälkeen ja kaikki tapahtuu automaattisesti. Katso alla olevat koodiesimerkit ja lopputulokset.

```Javascript
// Luodaan päivämääräoliot
let currentDate = new Date();
let myBirthday = new Date(1990, 3, 24);

// Muunnetaan päivämäärät merkkijonoksi
let currentDateStr = currentDate.toString();
let myBirthdayStr = myBirthday.toString();

// Tulostetaan merkkijonot
console.log(currentDateStr); // Su Sep 05 2021 17: 08: 41 GMT + 0300 (Eastern European Summer Time)
console.log(myBirthdayStr); // Ti Huhti 24 1990 00: 00: 00 GMT + 0300 (Eastern European Summer Time)
```

Syntaksin määrittelyn mukaan "toString ()" -metodi palauttaa päivämäärän "päiväysaikana", joka näyttää päivämäärän ja ajan sekä aikavyöhykkeen. Tämä formaatti vaihtelee kuitenkin käyttöjärjestelmän ja sijainnin mukaan. Voit muuttaa päivämäärän esitystapaa käyttämällä erilaisia JavaScriptin päivämäärämuotoiluja, kuten "toDateString ()" tai "toLocaleDateString ()".

Syvemmälle päivämäärän muuntamiseen merkkijonoksi

Päivämäärän muuntaminen merkkijonoksi voi olla monimutkaisempaa, jos aiomme esittää sen eri kielillä tai käyttää tiettyä päivämääräformaattia. Jotkut maat käyttävät esimerkiksi erilaisia ​​päivämääräformaatteja, kuten päivä/kuukausi/vuosi tai kuukausi/päivä/vuosi. JavaScript tarjoaa kuitenkin ratkaisun tähän ongelmaan "toLocaleString ()" -metodin avulla. Tämä metodi antaa meille mahdollisuuden määrittää aikavyöhyke ja kieliasetukset, jotta voimme tarkasti näyttää päivämäärätietoja halutulla tavalla.

```Javascript
// Luodaan päivämääräoliot
let currentDate = new Date();
let myBirthday = new Date(1990, 3, 24);

// Määritetään kieliasetukset
let options = { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' };

// Tulostetaan päivämäärät
console.log(currentDate.toLocaleString('de-DE', options)); // Sonntag, 5 September 2021
console.log(myBirthday.toLocaleString('fr-FR', options)); // Mardi 24 Mars 1990
```

Tässä esimerkissä käytämme "lang-XX" -arvoja määritelläksemme halutun kielen. Voit käyttää myös muita optioita, kuten "hour", "minute" ja "second" näyttääksesi ajan. "Hour12: true" -asetuksella voimme myös määrittää, että näytettävä aika muunnetaan 12-t