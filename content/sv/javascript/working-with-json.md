---
title:                "Arbeta med JSON"
html_title:           "Arduino: Arbeta med JSON"
simple_title:         "Arbeta med JSON"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
I JavaScript hanterar vi ofta JSON (JavaScript Object Notation) för att lagra och utbyta data. Det är smidigt för att det är textbaserat och lätt att läsa för människor, samtidigt som det är enkelt för maskiner att generera och tolka.

## How to:
### Skapa JSON-sträng från objekt
```Javascript
let objekt = { namn: "Anna", ålder: 25, yrke: "Utvecklare" };
let jsonSträng = JSON.stringify(objekt);
console.log(jsonSträng); // Output: '{"namn":"Anna","ålder":25,"yrke":"Utvecklare"}'
```

### Parse JSON-sträng till objekt
```Javascript
let jsonSträng = '{"namn":"Anna","ålder":25,"yrke":"Utvecklare"}';
let objekt = JSON.parse(jsonSträng);
console.log(objekt); // Output: { namn: 'Anna', ålder: 25, yrke: 'Utvecklare' }
```

## Deep Dive
JSON introducerades i början av 2000-talet och fick snabbt stor spridning på grund av sin enkelhet jämfört med XML. XML används fortfarande, särskilt där dokumentformat är viktigt, men JSON har blivit standarden för webb-API:er på grund av sin kompakthet och hastighet. Vid implementation måste man vara medveten om säkerhetsaspekter, som att validera och rensa JSON-data för att undvika säkerhetshot såsom XSS-attacker.

## See Also
- MDN:s dokumentation om JSON: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON)
- En jämförelse mellan JSON och XML: [https://www.json.org/xml.html](https://www.json.org/xml.html)