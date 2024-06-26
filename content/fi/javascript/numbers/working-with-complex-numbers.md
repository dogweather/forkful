---
date: 2024-01-26 04:42:23.900526-07:00
description: "Kuinka: JavaScript ei sis\xE4ll\xE4 valmiina tukea kompleksiluvuille,\
  \ mutta voit k\xE4\xE4ri\xE4 hihasi ja k\xE4sitell\xE4 niit\xE4 itse objektien ja\
  \ matematiikan avulla. T\xE4ss\xE4\u2026"
lastmod: '2024-03-13T22:44:56.943825-06:00'
model: gpt-4-0125-preview
summary: "JavaScript ei sis\xE4ll\xE4 valmiina tukea kompleksiluvuille, mutta voit\
  \ k\xE4\xE4ri\xE4 hihasi ja k\xE4sitell\xE4 niit\xE4 itse objektien ja matematiikan\
  \ avulla."
title: "Kompleksilukujen k\xE4sittely"
weight: 14
---

## Kuinka:
JavaScript ei sisällä valmiina tukea kompleksiluvuille, mutta voit kääriä hihasi ja käsitellä niitä itse objektien ja matematiikan avulla. Tässä on nopea esimerkki.

```javascript
class ComplexNumber {
  constructor(real, imaginary) {
    this.real = real;
    this.imaginary = imaginary;
  }

  add(other) {
    return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
  }

  // ...lisää tarvittaessa lisää metodeja (vähennä, kerro, jaa)

  toString() {
    return `${this.real} + ${this.imaginary}i`;
  }
}

const a = new ComplexNumber(1, 2);
const b = new ComplexNumber(3, 4);
const tulos = a.add(b);

console.log(`Tulos: ${tulos}`); // Tulos: 4 + 6i
```

## Syväsukellus
Kompleksiluvut ovat olleet olemassa jo 1500-luvulta lähtien, kiitos italialaisen matemaatikon Gerolamo Cardanon. Ne ovat tulleet välttämättömiksi monilla aloilla, kuten insinööritieteissä ja fysiikassa. Nykyohjelmoinnissa ne ovat avainasemassa simulaatioissa ja algoritmeissa, jotka tarvitsevat moniulotteisuutta.

Nyt JavaScript ei ole alkujaan varustettu kompleksiluvuilla. Mutta oman tuunauksen lisäksi voit käyttää matematiikkakirjastoja, kuten math.js tai numeric.js. Nämä tarjoavat voimaa raskaampaan kompleksilukujen käsittelyyn, tuoden etuja kuten lisää operaatioita, suuruuden laskeminen ja argumentin löytäminen.

Hupun alla, kun toimit kompleksilukujen kanssa, on kuin hallitsisit kahta erillistä numeroa, jotka ovat yhteen kytkettyjä. Yhteen- ja vähennyslasku ovat suoria toimia - täsmäytä todelliset reaaliluvut keskenään, imaginaariset imaginaaristen kanssa. Kertolasku ja jakolasku muuttuvat mausteisemmiksi ristiintermien kanssa ja vaativat enemmän huomiota.

## Katso myös
- MDN Web Docs JavaScriptistä: https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript
- Math.js, matematiikkakirjasto, joka sisältää kompleksilukuja: https://mathjs.org/docs/datatypes/complex_numbers.html
- Numeric.js, toinen kirjasto: http://numericjs.com/documentation.html
- Syvempi sukellus kompleksilukuihin (matematiikkaan keskittyen): https://mathworld.wolfram.com/ComplexNumber.html
