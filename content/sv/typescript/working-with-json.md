---
title:                "Arbeta med json"
html_title:           "TypeScript: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## Vad är det och varför?

Att arbeta med JSON är en viktig del av utveckling i TypeScript. JSON, eller JavaScript Object Notation, är ett dataformat som används för att skicka och lagra data i en läsbar och strukturerad form. Det används ofta som ersättning för XML, eftersom det är lättare att läsa och skriva för människor.

Programmerare använder JSON för att kommunicera data mellan olika applikationer, som att hämta data från en API eller spara information i en databas. Det är också en populär metod för att lagra och överföra data i dokument och konfigurationsfiler.

## Hur man gör:

För att använda JSON i TypeScript, måste du först importera JSON-modulen genom att skriva `import * as JSON from 'json'` på toppen av din fil. Sedan kan du använda `JSON.parse()` för att konvertera en sträng i JSON-format till ett TypeScript-objekt, eller `JSON.stringify()` för att konvertera ett objekt till en JSON-sträng. Nedan följer några exempel på hur man kan använda det:

```TypeScript
// Exempel på att konvertera en JSON-sträng till ett objekt
let jsonStr: string = '{"namn": "Lisa", "ålder": 25}';
let objekt = JSON.parse(jsonStr); 
console.log(objekt.namn); // Output: Lisa
console.log(objekt.ålder); // Output: 25
```

```TypeScript
// Exempel på att konvertera ett objekt till en JSON-sträng
let person = {namn: "Lisa", ålder: 25};
let jsonStr = JSON.stringify(person);
console.log(jsonStr); // Output: {"namn":"Lisa","ålder":25}
```

## Djupdykning:

JSON skapades av Douglas Crockford i början av 2000-talet och används i stor utsträckning i webbutveckling. Det är lätt att läsa och skriva för människor, och lätt att tolka och generera för datorer. Alternativ till JSON inkluderar XML, YAML och TOML.

När du arbetar med JSON i TypeScript måste du följa strikta regler för den formatering och syntax som krävs för att det ska kunna tolkas korrekt. Det är också viktigt att hantera felaktiga eller ofullständiga JSON-data på ett säkert sätt för att undvika problem i din applikation.

## Se också:

- [Mozilla Developer Network - JSON](https://developer.mozilla.org/sv/docs/Web/JavaScript/Reference/Global_Objects/JSON)
- [JSON - Officiell hemsida](https://www.json.org/json-en.html)
- [Förstå JSON - En lättbegriplig guide](https://www.w3schools.com/js/js_json_intro.asp)