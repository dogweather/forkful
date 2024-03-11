---
date: 2024-01-26 04:23:45.329890-07:00
description: "TOML, skr\xF3t od Tom's Obvious, Minimal Language, okre\u015Bla, jak\
  \ strukturowa\u0107 pliki konfiguracyjne. Programi\u015Bci korzystaj\u0105 z TOML,\
  \ poniewa\u017C jest \u0142atwy do\u2026"
lastmod: '2024-03-11T00:14:09.031938-06:00'
model: gpt-4-0125-preview
summary: "TOML, skr\xF3t od Tom's Obvious, Minimal Language, okre\u015Bla, jak strukturowa\u0107\
  \ pliki konfiguracyjne. Programi\u015Bci korzystaj\u0105 z TOML, poniewa\u017C jest\
  \ \u0142atwy do\u2026"
title: Praca z TOML
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
