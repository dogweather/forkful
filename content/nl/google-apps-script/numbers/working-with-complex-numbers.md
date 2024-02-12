---
title:                "Werken met complexe getallen"
date:                  2024-02-01T22:07:38.793153-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met complexe getallen"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/google-apps-script/working-with-complex-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Complexe getallen, gerepresenteerd als een combinatie van reële en imaginaire eenheden (bijv. 3 + 4i), zijn fundamenteel in verschillende computationele problemen, vooral in engineering, natuurkunde en toegepaste wiskunde. Leren hoe je deze getallen kunt manipuleren in Google Apps Script stelt programmeurs in staat hun mogelijkheden uit te breiden naar wetenschappelijk rekenen, signaalverwerking en meer.

## Hoe te:
Google Apps Script heeft geen ingebouwde ondersteuning voor complexe getallen, wat de implementatie van aangepaste functionaliteit noodzakelijk maakt. Hieronder staat een basisstructuur voor het omgaan met complexe getallen, inclusief optellen, aftrekken en vermenigvuldigen.

```javascript
// Definieer een constructor voor complexe getallen
function Complex(reëel, imag) {
  this.reëel = reëel;
  this.imag = imag;
}

// Methode voor het optellen van twee complexe getallen
Complex.prototype.add = function(ander) {
  return new Complex(this.reëel + ander.reëel, this.imag + ander.imag);
};

// Methode voor het aftrekken van twee complexe getallen
Complex.prototype.subtract = function(ander) {
  return new Complex(this.reëel - ander.reëel, this.imag - ander.imag);
};

// Methode voor het vermenigvuldigen van twee complexe getallen
Complex.prototype.multiply = function(ander) {
  return new Complex(
    this.reëel * ander.reëel - this.imag * ander.imag,
    this.reëel * ander.imag + this.imag * ander.reëel
  );
};

// Voorbeeldgebruik
var num1 = new Complex(3, 4);
var num2 = new Complex(1, 2);

// Twee complexe getallen optellen
var som = num1.add(num2);
console.log(`Som: ${som.reëel} + ${som.imag}i`); // Som: 4 + 6i

// Twee complexe getallen aftrekken
var verschil = num1.subtract(num2);
console.log(`Verschil: ${verschil.reëel} + ${verschil.imag}i`); // Verschil: 2 + 2i

// Twee complexe getallen vermenigvuldigen
var product = num1.multiply(num2);
console.log(`Product: ${product.reëel} + ${product.imag}i`); // Product: -5 + 10i
```

## Diepere duik:
Het concept van complexe getallen gaat terug tot de 16e eeuw, maar het was het werk van wiskundigen als Euler en Gauss dat hun plaats in de wiskunde verstevigde. Ondanks hun nut, worden complexe getallen niet direct ondersteund in JavaScript of, bij uitbreiding, Google Apps Script. Het gebrek aan native ondersteuning betekent dat operaties op complexe getallen handmatig geïmplementeerd moeten worden, zoals gedemonstreerd. Hoewel dit een goede leeropportunity biedt en voldoende functionaliteit voor basisbehoeften, kan men voor zwaar computationeel werk dat complexe getallen vereist, overwegen om andere programmeeromgevingen die beter geschikt zijn voor wiskundig rekenen te gebruiken, zoals Python met NumPy, die ingebouwde, zeer geoptimaliseerde operaties bieden voor het hanteren van complexe getallen. Desalniettemin is het begrijpen en implementeren van basisoperaties in Google Apps Script een nuttige oefening voor diegenen die hun programmeervaardigheden willen verbreden en toepassen in een breed scala aan contexten.
