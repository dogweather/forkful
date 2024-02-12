---
title:                "Praca z TOML"
aliases:
- /pl/javascript/working-with-toml/
date:                  2024-01-26T04:23:45.329890-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/working-with-toml.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
TOML, skrót od Tom's Obvious, Minimal Language, określa, jak strukturować pliki konfiguracyjne. Programiści korzystają z TOML, ponieważ jest łatwy do odczytania, zapisu i dobrze mapuje się na tablicę mieszającą, co sprawia, że jest to preferowany wybór dla konfiguracji.

## Jak to zrobić:
Aby pracować z TOML w JavaScript, potrzebny będzie parser, taki jak `@iarna/toml`. Na początek zainstaluj go: `npm install @iarna/toml`. Następnie, sparsuj ciąg TOML do obiektu JavaScript lub przekształć obiekt JavaScript do formatu TOML.

```javascript
const toml = require('@iarna/toml');

// Parsowanie ciągu TOML na obiekt JS
const tomlStr = `
title = "Przykład TOML"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
`;

const parsedData = toml.parse(tomlStr);
console.log(parsedData);

// Konwersja obiektu JS na ciąg TOML
const jsObject = {
  title: "Przykład TOML",
  database: {
    server: "192.168.1.1",
    ports: [8001, 8001, 8002]
  }
};

const tomlString = toml.stringify(jsObject);
console.log(tomlString);
```

## Szczegółowe omówienie
TOML został po raz pierwszy wydany w 2013 roku przez Toma Preston-Wernera, współzałożyciela GitHuba. Został zaprojektowany, aby zastąpić inne formaty, takie jak INI, będąc bardziej ustandaryzowanym i łatwiejszym do sparsowania. JSON i YAML to alternatywy, ale mogą być zbyt skomplikowane lub zbyt elastyczne. Przewaga TOML polega na statycznej konfiguracji, gdzie preferowany jest prosty, jasny format. Jego projekt umożliwia proste mapowanie na tablicę mieszającą, z kluczami i wartościami odpowiadającymi nazwom właściwości i ich wartościom. Dla szerszego przyjęcia, możesz potrzebować zintegrować narzędzia, które mogą konwertować pomiędzy TOML a innymi formatami ze względu na różnorodne wsparcie w ekosystemie.

## Zobacz również
- Oficjalne repozytorium GitHub TOML: https://github.com/toml-lang/toml
- Porównanie TOML vs. YAML vs. JSON: https://gist.github.com/oconnor663/9aeb4ed56394cb013a20
- pakiet npm `@iarna/toml`: https://www.npmjs.com/package/@iarna/toml
