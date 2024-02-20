---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:59.189545-07:00
description: "De lengte van een string vinden betekent het tellen van de tekens. Programmeurs\
  \ doen dit om invoer te valideren, door strings te lopen en tekstgegevens\u2026"
lastmod: 2024-02-19 22:05:10.278187
model: gpt-4-0125-preview
summary: "De lengte van een string vinden betekent het tellen van de tekens. Programmeurs\
  \ doen dit om invoer te valideren, door strings te lopen en tekstgegevens\u2026"
title: De lengte van een string vinden
---

{{< edit_this_page >}}

## Wat & Waarom?
De lengte van een string vinden betekent het tellen van de tekens. Programmeurs doen dit om invoer te valideren, door strings te lopen en tekstgegevens efficiënt te manipuleren.

## Hoe:
JavaScript maakt het eenvoudig met de `.length` eigenschap.

```javascript
let groet = 'Hallo, wereld!';
console.log(groet.length); // Output: 13
```

Een lege string is gelijk aan nul:

```javascript
let leeg = '';
console.log(leeg.length); // Output: 0
```

Zelfs spaties tellen mee:

```javascript
let spaties = '   ';
console.log(spaties.length); // Output: 3
```

## Diepere duik
De `.length` eigenschap bestaat al sinds de eerste dagen van JS. Het is snel, omdat het niet echt een functie is, maar een instantie-eigenschap die wordt opgeslagen met het string-object.

Alternatieven, zoals handmatig door elk teken lopen om ze te tellen, bestaan, maar die zijn als het nemen van de trap in plaats van de lift - gebruik ze alleen wanneer nodig.

JavaScript behandelt strings als onveranderlijk, wat betekent dat `.length` niet verandert tenzij je een nieuwe string aan de variabele toewijst. De lengte wordt berekend wanneer de string wordt gemaakt.

Wat implementatie betreft, denk aan Unicode. Sommige tekens (zoals emoji of bepaalde taalalfabetten) worden mogelijk vertegenwoordigd door twee of meer code-eenheden in de UTF-16-codering van JavaScript:

```javascript
let glimlach = '😊';
console.log(glimlach.length); // Output: 2
```

Ook al lijkt het op één teken, sommige kunnen als twee "lengtes" tellen vanwege hoe ze zijn gecodeerd. Gewoon iets om te onthouden als je te maken hebt met diverse tekensets!

## Zie ook
- [MDN Web Docs - String.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Unicode en JavaScript strings](https://mathiasbynens.be/notes/javascript-unicode)
- [JavaScript string- en tekenencodings](https://flaviocopes.com/javascript-unicode/)
