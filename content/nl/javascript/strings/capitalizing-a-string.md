---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:12.232368-07:00
description: "Een string kapitaliseren betekent het veranderen van de eerste letter\
  \ van een woord naar een hoofdletter. Programmeurs doen dit om taalconventies te\u2026"
lastmod: '2024-03-13T22:44:51.183174-06:00'
model: gpt-4-0125-preview
summary: Een string kapitaliseren betekent het veranderen van de eerste letter van
  een woord naar een hoofdletter.
title: Een string met hoofdletters maken
weight: 2
---

## Hoe doe je het:
JavaScript heeft geen ingebouwde methode om te kapitaliseren, maar hier is een eenvoudige functie die het voor elkaar krijgt:

```javascript
function capitalizeFirstLetter(string) {
    return string.charAt(0).toUpperCase() + string.slice(1);
}

console.log(capitalizeFirstLetter('hallo')); // Output: Hallo
```

Voor meerdere woorden:

```javascript
function capitalizeWords(str) {
    return str.split(' ').map(capitalizeFirstLetter).join(' ');
}

console.log(capitalizeWords('hallo wereld!')); // Output: Hallo Wereld!
```

## Diepgaand
Het kapitaliseren van strings had niet altijd ingebouwde functies in talen en omvatte vaak handmatige ASCII-manipulatie. Tegenwoordig bieden de meeste programmeertalen methoden voor stringmanipulatie, maar JavaScript vereist een meer doe-het-zelf aanpak.

### Alternatieven:
Je zou CSS kunnen gebruiken om tekst op webpagina's te kapitaliseren (`text-transform: capitalize;`), of bibliotheken zoals Lodash hebben kapitaliseerfuncties. Maar het doen met zuiver JavaScript, zoals hierboven getoond, heeft geen afhankelijkheden.

### Implementatie Details:
`charAt(0)` pakt het eerste karakter. `toUpperCase()` maakt het een hoofdletter. Dit combineren met de rest van de string `slice(1)` geeft je een gekapitaliseerde string. Deze methode werkt goed ervan uitgaande dat de invoer een string is en niet begint met een spatie.

## Zie Ook:
- MDN's text-transform CSS voor kapitalisatie: https://developer.mozilla.org/en-US/docs/Web/CSS/text-transform
- Lodash's kapitaliseermethode documentatie: https://lodash.com/docs/4.17.15#capitalize
- JavaScript String.prototype.toUpperCase(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase
