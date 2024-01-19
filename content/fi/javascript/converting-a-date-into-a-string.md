---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Go: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Muunnetaan päivämäärä merkkijonoksi JavaScriptillä. Miksi? Koska meidän täytyy esittää päivämäärät ymmärrettävässä muodossa ihmisille tai tallentaa ne järjestelmille, jotka voivat käsitellä vain merkkijonoja.

## Miten:

Tässä on koodiesimerkki, joka muuntaa päivämäärän merkkijonoksi JavaScriptillä:

```Javascript
var today = new Date();
var stringDate = today.toString();
console.log(stringDate);
```

Tuotanto näyttäisi tältä:

```Javascript
"Wed Sep 22 2021 11:07:34 GMT+0300 (Eastern European Summer Time)"
```

## Syvällisemmin:

### Historiallinen Konteksti:
JavaScriptin alkuaikoina päivämäärän esittäminen tietyssä muodossa oli haastavaa. 'Date' oli yksi JavaScriptin alkuperäisistä globaaleista olioista. 

### Vaihtoehdot:
JavaScriptillä on erilaisia tapoja käsitellä päivämääriä ja muuntaa ne merkkijonoiksi. Voit muuttaa päivämäärän merkkijonoksi käyttäen `toDateString()`, `toISOString()` tai `toLocaleString()` metodeja.

```Javascript
var currentDay = new Date();
console.log(currentDay.toDateString()); // "Wed Sep 22 2021"
console.log(currentDay.toISOString()); // "2021-09-22T08:07:34.336Z"
console.log(currentDay.toLocaleString()); // "22.9.2021 klo 11.07.34"
```

### Toteutus:
Kun muunnat päivämäärän merkkijonoksi JavaScriptillä, päivämäärä kapseloidaan merkkijonoksi alkuperäisen päivämääräolion ulkopuolella, säilyttäen alkuperäisen päivämääräolion muuttamattomana.

## Katso Myös:
1. [MDN Web Docs: Date](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Date)
2. [W3Schools: JavaScript Date Reference](https://www.w3schools.com/jsref/jsref_obj_date.asp)
3. [JavaScript.Info: Date and Time](https://javascript.info/date)
4. [JavaScript Kit: Date and Time in Javascript](http://www.javascriptkit.com/javatutors/datetime.shtml)