---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:58.203200-07:00
description: "TOML, een afkorting voor Tom's Obvious, Minimal Language, definieert\
  \ hoe configuratiebestanden moeten worden gestructureerd. Programmeurs werken met\
  \ TOML\u2026"
lastmod: '2024-03-13T22:44:51.226030-06:00'
model: gpt-4-0125-preview
summary: "TOML, een afkorting voor Tom's Obvious, Minimal Language, definieert hoe\
  \ configuratiebestanden moeten worden gestructureerd. Programmeurs werken met TOML\u2026"
title: Werken met TOML
weight: 39
---

## Wat & Waarom?
TOML, een afkorting voor Tom's Obvious, Minimal Language, definieert hoe configuratiebestanden moeten worden gestructureerd. Programmeurs werken met TOML omdat het makkelijk te lezen, te schrijven is, en goed in kaart kan worden gebracht op een hashtabel, waardoor het de voorkeur geniet voor configuraties.

## Hoe te:
Om met TOML in JavaScript te werken, heb je een parser zoals `@iarna/toml` nodig. Installeer het eerst: `npm install @iarna/toml`. Parse vervolgens een TOML-string naar een JavaScript-object of zet een JavaScript-object om naar TOML-formaat.

```javascript
const toml = require('@iarna/toml');

// Parse TOML-string naar JS-object
const tomlStr = `
title = "Voorbeeld van TOML"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
`;

const parsedData = toml.parse(tomlStr);
console.log(parsedData);

// Converteer JS-object naar TOML-string
const jsObject = {
  title: "Voorbeeld van TOML",
  database: {
    server: "192.168.1.1",
    ports: [8001, 8001, 8002]
  }
};

const tomlString = toml.stringify(jsObject);
console.log(tomlString);
```

## Diepere Duik
TOML werd voor het eerst uitgebracht in 2013 door Tom Preston-Werner, een mede-oprichter van GitHub. Het is ontworpen om andere formaten, zoals INI, te vervangen door meer gestandaardiseerd en makkelijker te parsen te zijn. Alternatieven zoals JSON en YAML kunnen te complex of te flexibel zijn. Het voordeel van TOML ligt in statische configuratie waar een eenvoudig, duidelijk formaat de voorkeur heeft. Het ontwerp maakt een eenvoudige in kaart brenging op een hashtabel mogelijk, met sleutels en waarden die overeenkomen met eigenschapsnamen en hun waarden. Voor bredere adoptie moet je misschien tools integreren die kunnen converteren tussen TOML en andere formaten vanwege variërende ondersteuning in het ecosysteem.

## Zie Ook
- De officiële TOML GitHub-repository: https://github.com/toml-lang/toml
- Vergelijking tussen TOML vs. YAML vs. JSON: https://gist.github.com/oconnor663/9aeb4ed56394cb013a20
- npm `@iarna/toml` pakket: https://www.npmjs.com/package/@iarna/toml
