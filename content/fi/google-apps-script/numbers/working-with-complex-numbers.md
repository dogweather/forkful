---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:25.969798-07:00
description: "Kuinka: Google Apps Script ei tue kompleksilukuja sis\xE4\xE4nrakennetusti,\
  \ mik\xE4 edellytt\xE4\xE4 mukautetun toiminnallisuuden toteuttamista. Alla on perusrakenne\u2026"
lastmod: '2024-03-13T22:44:56.089784-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script ei tue kompleksilukuja sis\xE4\xE4nrakennetusti, mik\xE4\
  \ edellytt\xE4\xE4 mukautetun toiminnallisuuden toteuttamista."
title: "Ty\xF6skentely kompleksilukujen kanssa"
weight: 14
---

## Kuinka:
Google Apps Script ei tue kompleksilukuja sisäänrakennetusti, mikä edellyttää mukautetun toiminnallisuuden toteuttamista. Alla on perusrakenne kompleksilukujen käsittelyyn, mukaan lukien yhteenlasku, vähennyslasku ja kertolasku.

```javascript
// Määrittele kompleksilukujen konstruktori
function Complex(real, imag) {
  this.real = real;
  this.imag = imag;
}

// Menetelmä kahden kompleksiluvun yhteenlaskemiseksi
Complex.prototype.add = function(other) {
  return new Complex(this.real + other.real, this.imag + other.imag);
};

// Menetelmä kahden kompleksiluvun vähentämiseksi
Complex.prototype.subtract = function(other) {
  return new Complex(this.real - other.real, this.imag - other.imag);
};

// Menetelmä kahden kompleksiluvun kertomiseksi
Complex.prototype.multiply = function(other) {
  return new Complex(
    this.real * other.real - this.imag * other.imag,
    this.real * other.imag + this.imag * other.real
  );
};

// Esimerkkinä käyttö
var num1 = new Complex(3, 4);
var num2 = new Complex(1, 2);

// Lisää kaksi kompleksilukua
var sum = num1.add(num2);
console.log(`Summa: ${sum.real} + ${sum.imag}i`); // Summa: 4 + 6i

// Vähennä kaksi kompleksilukua
var difference = num1.subtract(num2);
console.log(`Ero: ${difference.real} + ${difference.imag}i`); // Ero: 2 + 2i

// Kerro kaksi kompleksilukua
var product = num1.multiply(num2);
console.log(`Tulo: ${product.real} + ${product.imag}i`); // Tulo: -5 + 10i
```

## Syväsukellus:
Kompleksilukujen käsite juontaa juurensa 1500-luvulle, mutta matemaatikkojen kuten Eulerin ja Gaussin työ vakiinnutti niiden paikan matematiikassa. Huolimatta niiden hyödyllisyydestä, kompleksiluvut eivät ole suoraan tuettuja JavaScriptissa tai siten Google Apps Scriptissä. Natiivin tuen puuttuminen tarkoittaa, että kompleksilukutoimintoja on toteutettava manuaalisesti, kuten esimerkki osoittaa. Vaikka tämä tarjoaa hyvän oppimismahdollisuuden ja riittävän toiminnallisuuden perustarpeisiin, raskaaseen laskentatyöhön kompleksilukujen kanssa saatetaan harkita muiden ohjelmointiympäristöjen, kuten Pythonin ja NumPyn, käyttämistä, jotka tarjoavat sisäänrakennettuja, erittäin optimoituja toimintoja kompleksilukujen käsittelyyn. Siitä huolimatta perustoimintojen ymmärtäminen ja toteuttaminen Google Apps Scriptillä on hyödyllinen harjoitus niille, jotka haluavat laajentaa ohjelmointitaitojaan ja soveltaa niitä laajassa kontekstissa.
