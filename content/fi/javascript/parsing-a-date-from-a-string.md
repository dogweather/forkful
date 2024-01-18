---
title:                "Muotoiltu päivämäärä merkkijonosta"
html_title:           "Javascript: Muotoiltu päivämäärä merkkijonosta"
simple_title:         "Muotoiltu päivämäärä merkkijonosta"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?
Päivämäärän muuttaminen merkkijonosta Javascript-koodissa tarkoittaa päivämäärätiedon hakemista merkkijonosta ja sen muuntamista Javascriptin Date-objektiksi. Tämä on tärkeää esimerkiksi silloin, kun halutaan näyttää tai vertailla päivämääriä ohjelmassa.

# Miten:
Päivämäärän muuttaminen merkkijonosta on helppoa käyttäen Javascriptin sisäänrakennettua Date() -metodia. Alla on esimerkki koodinpätkä, joka muuntaa merkkijonona olevan päivämäärän Date-objektiksi:

```Javascript
let dateString = "12/31/2020";
let dateObj = new Date(dateString);
console.log(dateObj);
// Output: Wed Dec 31 2020 00:00:00 GMT-0500 (Eastern Standard Time)
```

# Syväsukellus:
Päivämäärän muuttaminen merkkijonosta on ollut tärkeää jo varhaisista Javascriptin versioista lähtien. Nykyään on olemassa myös muita tapoja käsitellä päivämääriä, kuten esimerkiksi Moment.js-kirjasto. Päivämäärän muuttamisen mekanismi vaihtelee myös eri ohjelmointikielissä, joten kannattaa aina ensin tarkistaa käytössä olevan kielen dokumentaatio.

# Katso myös:
- MDN Web Docs: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js: https://momentjs.com/