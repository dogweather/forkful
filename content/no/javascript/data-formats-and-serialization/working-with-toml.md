---
date: 2024-01-26 04:23:29.999267-07:00
description: "Hvordan: For \xE5 jobbe med TOML i JavaScript, trenger du en parser\
  \ som `@iarna/toml`. F\xF8rst, installer den: `npm install @iarna/toml`. Deretter,\
  \ parser du\u2026"
lastmod: '2024-03-13T22:44:41.206767-06:00'
model: gpt-4-0125-preview
summary: "For \xE5 jobbe med TOML i JavaScript, trenger du en parser som `@iarna/toml`."
title: Jobbe med TOML
weight: 39
---

## Hvordan:
For å jobbe med TOML i JavaScript, trenger du en parser som `@iarna/toml`. Først, installer den: `npm install @iarna/toml`. Deretter, parser du en TOML-streng til et JavaScript-objekt eller lager en streng av et JavaScript-objekt i TOML-format.

```javascript
const toml = require('@iarna/toml');

// Parse TOML-streng til JS-objekt
const tomlStr = `
title = "TOML Eksempel"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
`;

const parsedData = toml.parse(tomlStr);
console.log(parsedData);

// Konverter JS-objekt til TOML-streng
const jsObject = {
  title: "TOML Eksempel",
  database: {
    server: "192.168.1.1",
    ports: [8001, 8001, 8002]
  }
};

const tomlString = toml.stringify(jsObject);
console.log(tomlString);
```

## Dypdykk
TOML ble først utgitt i 2013 av Tom Preston-Werner, en av grunnleggerne av GitHub. Det ble designet for å overta for andre formater, som INI, ved å være mer standardisert og lettere å parse. JSON og YAML er alternativer, men kan være for komplekse eller for fleksible. TOMLs fordel er i statisk konfigurering der et enkelt, klart format foretrekkes. Dets design tillater enkel kartlegging til en hashtabell, med nøkler og verdier som tilsvarer egenskapsnavn og deres verdier. For bredere adopsjon, kan det være nødvendig å integrere verktøy som kan konvertere mellom TOML og andre formater på grunn av varierende økosystemstøtte.

## Se også
- Det offisielle TOML GitHub-repositoriet: https://github.com/toml-lang/toml
- Sammenligning av TOML vs. YAML vs. JSON: https://gist.github.com/oconnor663/9aeb4ed56394cb013a20
- npm-pakken `@iarna/toml`: https://www.npmjs.com/package/@iarna/toml
