---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Go: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Muunnetaan päiväys merkkijonoksi tarkoittaa, että ohjelmointikielellämme koodataan päiväys formaattiin, jota voidaan käsitellä tekstinä. Tätä tekniikkaa käytetään usein dataa jäsennetttäessä tai käytettäessä menetelmiä, jotka hyväksyvät vain merkkejä.

## Kuinka tehdä:

```TypeScript
let pvm = new Date();
let pvmMerkkijono = pvm.toString();
console.log(pvmMerkkijono);
```

Esimerkkejä tulosteista voidaan olla:
```
Wed Sep 15 2021 16:25:47 GMT+0300 (Itä-Euroopan kesäaika)
```

Voit myös muotoilla päivämäärän haluamasi mukaisesti.

```TypeScript
let pvm = new Date();
let dd = String(pvm.getDate()).padStart(2, '0');
let mm = String(pvm.getMonth() + 1).padStart(2, '0'); //Tammikuu on 0!
let yyyy = pvm.getFullYear();
let pvmMerkkijono = dd + '/' + mm + '/' + yyyy;
console.log(pvmMerkkijono);
```

Esimerkkejä tulosteista voidaan olla:
```
15/09/2021
```

## Syvä sukellus

Historiallisessa kontekstissa, päivämäärien muuttaminen merkkijonoiksi on ollut käytössä melkein yhtä kauan kuin ohjelmointi itse. Tämä johtuu luultavasti siitä, että monet vanhat tietokonejärjestelmät ja ohjelmointikielet eivät olleet erityisen hyviä päivämäärien ja aikojen käsittelyssä.

Vaihtoehtoisesti, toiset formaatit, kuten JSON, voivat automaattisesti eritellä päiväysolion merkkijonoksi, kun päiväys lähetetään JSON-muodossa.

TypeScript, kuten useimmat modernit ohjelmointikielet, tekee tämän prosessin suhteellisen mutkattomaksi. `toString()` funktio on osa JavaScriptin `Date` objektia, joten kun TypeScript-koodi muunnetaan JavaScriptiksi, `Date.toString()` toimii samalla tavalla molemmissa.

## Katso myös:

- MDN Web Docs, Date.prototype.toString(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toString 
- Stack Overflow, Convert JS date time to YYYY-MM-DD HH:MM:SS: https://stackoverflow.com/questions/3605214/convert-js-date-time-to-yyyy-mm-dd-hhmmss
- TypeScript HandBook, Date Object: https://www.typescriptlang.org/docs/handbook/declaration-files/do-s-and-don-ts.html