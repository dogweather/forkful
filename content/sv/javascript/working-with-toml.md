---
title:                "Att arbeta med TOML"
date:                  2024-01-26T04:23:53.298516-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att arbeta med TOML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/working-with-toml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
TOML, som står för Tom's Obvious, Minimal Language, definierar hur konfigurationsfiler ska struktureras. Programmerare arbetar med TOML eftersom det är lätt att läsa, skriva och mappar fint till en hashtabell, vilket gör det till ett givet val för konfigurationer.

## Hur man gör:
För att arbeta med TOML i JavaScript behöver du en parser som `@iarna/toml`. Först, installera den: `npm install @iarna/toml`. Sedan, tolka en TOML-sträng till ett JavaScript-objekt eller konvertera ett JavaScript-objekt till TOML-format.

```javascript
const toml = require('@iarna/toml');

// Tolka TOML-sträng till JS-objekt
const tomlStr = `
title = "TOML Example"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
`;

const parsedData = toml.parse(tomlStr);
console.log(parsedData);

// Konvertera JS-objekt till TOML-sträng
const jsObject = {
  title: "TOML Example",
  database: {
    server: "192.168.1.1",
    ports: [8001, 8001, 8002]
  }
};

const tomlString = toml.stringify(jsObject);
console.log(tomlString);
```

## Fördjupning
TOML släpptes först 2013 av Tom Preston-Werner, en medgrundare av GitHub. Det designades för att ersätta andra format, som INI, genom att vara mer standardiserat och lättare att tolka. Alternativ som JSON och YAML kan vara för komplexa eller för flexibla. TOML:s fördel är inom statisk konfiguration där ett enkelt, tydligt format är att föredra. Dess design möjliggör enkel mappning till en hashtabell, med nycklar och värden som motsvarar egenskapsnamn och deras värden. För bredare acceptans kan det behövas integration av verktyg som kan konvertera mellan TOML och andra format på grund av varierande ekosystemstöd.

## Se även
- Det officiella TOML GitHub-repositoriet: https://github.com/toml-lang/toml
- Jämförelse mellan TOML vs. YAML vs. JSON: https://gist.github.com/oconnor663/9aeb4ed56394cb013a20
- npm-paketet `@iarna/toml`: https://www.npmjs.com/package/@iarna/toml