---
date: 2024-01-20 17:45:56.336635-07:00
description: "Substringit ovat merkkijonoista poimittuja osajonoja. Ne auttavat meit\xE4\
  \ ty\xF6st\xE4m\xE4\xE4n ja analysoimaan teksti\xE4 tarpeen mukaan."
lastmod: '2024-03-13T22:44:56.939031-06:00'
model: gpt-4-1106-preview
summary: "Substringit ovat merkkijonoista poimittuja osajonoja. Ne auttavat meit\xE4\
  \ ty\xF6st\xE4m\xE4\xE4n ja analysoimaan teksti\xE4 tarpeen mukaan."
title: Merkkijonojen osien poimiminen
---

{{< edit_this_page >}}

## What & Why? - Mitä ja Miksi?
Substringit ovat merkkijonoista poimittuja osajonoja. Ne auttavat meitä työstämään ja analysoimaan tekstiä tarpeen mukaan.

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
