---
date: 2024-01-20 17:45:56.336635-07:00
description: "How to: - Kuinka: Substringit ovat vanhastaan osa ohjelmointia. JavaScriptiss\xE4\
  \ on useita tapoja: `substr`, `substring` ja `slice`. - Historiallisesti\u2026"
lastmod: '2024-04-05T21:53:58.517542-06:00'
model: gpt-4-1106-preview
summary: ''
title: Merkkijonojen osien poimiminen
weight: 6
---

## How to: - Kuinka:
```Javascript
let fullString = "Hei maailma!";
let partOfIt = fullString.substring(4, 10);

console.log(partOfIt); // Tulostaa: maailm

// Aloita alusta, ota neljä merkkiä
let fromStart = fullString.substr(0, 4);

console.log(fromStart); // Tulostaa: Hei 

// Käytä slice-menetelmää alkaen indeksistä 8
let slicedString = fullString.slice(8);

console.log(slicedString); // Tulostaa: ilma!
```

## Deep Dive - Sukellus Syvyyksiin:
Substringit ovat vanhastaan osa ohjelmointia. JavaScriptissä on useita tapoja: `substr`, `substring` ja `slice`. 

- Historiallisesti `substring` ja `substr` olivat päämenetelmiä, mutta `substr` on vanhentunut.
- `slice` on joustava: se toimii myös taulukoissa ja sallii negatiiviset indeksit.
- Suorituskyvyssä erot ovat nykyisin minimaalisia, mutta eri selaimissa tilanne on saattanut vaihdella.

## See Also - Katso Myös:
- MDN Web Docs: String methods - [MDN String metodit](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- JavaScript Kit: Substring Extraction - [JavaScript Substring](http://www.javascriptkit.com/javatutors/string3.shtml)
- Stack Overflow: When to use `substring` vs `substr` - [Stack Overflow keskustelu](https://stackoverflow.com/questions/3745515/what-is-the-difference-between-substr-and-substring)
