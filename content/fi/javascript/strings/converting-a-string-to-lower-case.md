---
date: 2024-01-20 17:39:09.654545-07:00
description: "How to: - N\xE4in teet: JavaScript tarjoaa muutaman suoranaisen keinon\
  \ tehd\xE4 merkkijonon muunnoksen. `toLowerCase()` on suosituin."
lastmod: '2024-04-05T21:53:58.515507-06:00'
model: gpt-4-1106-preview
summary: ''
title: Merkkijonon muuntaminen pieniksi kirjaimiksi
weight: 4
---

## How to: - Näin teet:
JavaScript tarjoaa muutaman suoranaisen keinon tehdä merkkijonon muunnoksen. `toLowerCase()` on suosituin.

```javascript
let lause = "Hei Maailma!";
let pienetKirjaimet = lause.toLowerCase();

console.log(pienetKirjaimet); // "hei maailma!"
```

Tämä tuottaa aina uuden merkkijonon ilman alkuperäisen muuttamista.

## Deep Dive - Syvä Sukellus:
JavaScript versiossa ES5 esiteltiin `String.prototype.toLowerCase()`, mikä on nopea ja vaivaton tapa muuttaa merkkijonot pieniksi kirjaimiksi. Historiallisesti tämä metodi on juontanut juurensa ohjelmointikielen C `tolower` funktiosta.

Vaihtoehtoja? Voit itse kirjoittaa loopin, joka käy läpi merkkijonon ja vaihtaa jokaisen kirjaimen pieneksi ASCII-taulukoiden kautta. Mutta miksi tehdä niin, kun on olemassa valmiita funktioita?

JavaScript toteuttaa Unicode-standardia, mikä tarkoittaa että `toLowerCase()` ymmärtää ja käsittelee myös esim. ääkkösiä. Kuitenkin, erikoistapauksissa, kuten Turkkilaisessa käytössä, standardi `toLowerCase()` ei välttämättä vastaa paikallisia odotuksia. Näihin tilanteisiin voi olla tarpeen käyttää `toLocaleLowerCase()` metodia, joka huomioi kulttuurilliset erityispiirteet.

```javascript
let sananlasku = "Älä LUOTA ohjelmistoON ilman Testiä!";
console.log(sananlasku.toLocaleLowerCase('fi-FI')); // "älä luota ohjelmistoon ilman testiä!"
```

## See Also - Katso Myös:
Lisätietoa ja muuta aiheeseen liittyvää voit löytää seuraavista lähteistä:

- MDN Web Docs: String.prototype.toLowerCase() - https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase
- MDN Web Docs: String.prototype.toLocaleLowerCase() - https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase
- ECMAScript 2022 Language Specification: https://tc39.es/ecma262/
