---
title:                "Kompleksilukujen käsittely"
date:                  2024-01-26T04:42:23.900526-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kompleksilukujen käsittely"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Kompleksiluvut ovat lukuja, joilla on todellinen ja imaginaarinen osa (kuten 3 + 4i). Ne tulevat esiin monissa ohjelmointiongelmissa, erityisesti signaalinkäsittelyssä, kvanttilaskennassa ja polynomiyhtälöiden ratkaisemisessa. Ohjelmoijat käsittelevät niitä näiden tehtävien tehokkaaseen suorittamiseen.

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
