---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Javascript: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

Konvertoidaanko vanha päivämäärä uuteen merkkijonoon? Miksi ohjelmoijat tekevät näin?

Konvertointi päivämäärästä merkkijonoon on prosessi, jossa päivämäärä muunnetaan siihen liittyväksi tekstiksi. Tämä on tärkeä toiminto, jota ohjelmoijat tekevät esimerkiksi käyttöliittymien suunnittelussa ja datan tallentamisessa tietokantoihin.

### Kuinka tehdä:
```Javascript
const date = new Date(); // Luodaan uusi päivämääräobjekti
const dateString = date.toString(); // Muunnetaan päivämäärä muotoon merkkijono
console.log(dateString); // Tulostaa esimerkiksi: "Wed Aug 18 2021 13:45:27 GMT+0300 (Eastern European Summer Time)"
```

### Syvä sukellus:
Päivämäärän muuntaminen merkkijonoksi ei ole uusi idea, vaan sitä on tehty jo pitkään. On olemassa erilaisia tapoja tehdä tämä, kuten käyttämällä vanhempia Javscriptin funktioita, kuten `.toDateString()` tai `.toGMTString()`. Uudempi tapa on kuitenkin käyttää `toString()` metodia, johon voidaan antaa parametrina haluttu muoto merkkijonolle.

### Katso myös:
- [MDN web docs: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools: Javascript Date toString()](https://www.w3schools.com/jsref/jsref_tostring_date.asp)