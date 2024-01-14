---
title:    "Javascript: Päivämäärän muuttaminen merkkijonoksi"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# Miksi: Miksi muuttaa päivämäärä merkkijonoksi?

Päivämäärän muuttaminen merkkijonoksi on tärkeä osa Javascript-ohjelmointia, koska se antaa mahdollisuuden käsitellä päivämääriä kätevässä muodossa. Tämä on erityisen hyödyllistä, kun halutaan tallentaa tai näyttää päivämäärä tietokannassa tai sivustolla.

## Miten: Koodiesimerkkejä ja tulosteita

Jos haluat muuttaa päivämäärän merkkijonoksi Javascriptissä, voit tehdä sen yksinkertaisesti päivämäärä- ja aikaobjektin avulla.

```Javascript
// Luodaan uusi päivämäärä ja aika
let date = new Date();

// Muutetaan se merkkijonoksi
let dateString = date.toLocaleString();

// Tulostetaan merkkijono konsoliin
console.log(dateString);  // tulostaa esimerkiksi "15.4.2021 klo 12.00"
```

Voit myös asettaa haluamasi merkinnän päivämäärälle käyttämällä `toLocaleDateString()` ja `toLocaleTimeString()` -metodeja:

```Javascript
// Luodaan uusi päivämäärä ja aika
let date = new Date(2021, 3, 15, 12, 30);

// Muutetaan se merkkijonoksi halutun merkinnän mukaisesti
let dateString = date.toLocaleDateString('fi-FI', {weekday: 'long', year: 'numeric', month: 'long', day: 'numeric'});
let timeString = date.toLocaleTimeString('fi-FI', {hour: '2-digit', minute: '2-digit'});

// Tulostetaan muodostetut merkkijonot konsoliin
console.log(dateString); // tulostaa "torstai 15. huhtikuuta 2021"
console.log(timeString); // tulostaa "12.30"
```

## Syvällinen tarkastelu

Javascriptissä päivämäärät ja ajat tallennetaan erityisiksi objekteiksi, jotka tarjoavat monia hyödyllisiä metodeja, kuten `toLocaleString()` ja `toLocaleDateString()`. Nämä metodit ottavat parametreikseen maatunnisteen ja asetuksia, joiden avulla voit muokata päivämäärän merkintää haluamallasi tavalla.

On myös tärkeää muistaa, että päivämäärät käsitellään paikallisen aikavyöhykkeen mukaan, joten jos haluat käsitellä päivämääriä eri aikavyöhykkeiltä, sinun kannattaa käyttää `getUTCDate()` ja `setUTCDate()` -metodeja.

# Katso myös

Suosittelemme lukemaan lisää Javascriptin päivämäärä- ja aikaobjekteista näistä linkeistä:

- [MDN Web Docs: Date](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools: JavaScript Date Reference](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- [Eloquent Javascript: Dates and Times](https://eloquentjavascript.net/09_regexp.html#h_ebGV+0yWjv)

Kiitos lukemisesta! Toivottavasti tämä artikkeli auttoi sinua ymmärtämään, miten voit muuttaa päivämäärän merkkijonoksi Javascriptissä. Onnea ohjelmointiin!