---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:41.605910-07:00
description: "Jak to zrobi\u0107: W JavaScript praca z YAML zazwyczaj wi\u0105\u017C\
  e si\u0119 z u\u017Cyciem biblioteki firm trzecich, poniewa\u017C j\u0119zyk ten\
  \ nie zawiera wbudowanego parsera dla\u2026"
lastmod: '2024-03-13T22:44:35.816666-06:00'
model: gpt-4-0125-preview
summary: "W JavaScript praca z YAML zazwyczaj wi\u0105\u017Ce si\u0119 z u\u017Cyciem\
  \ biblioteki firm trzecich, poniewa\u017C j\u0119zyk ten nie zawiera wbudowanego\
  \ parsera dla YAML."
title: Praca z YAML
weight: 41
---

## Jak to zrobić:
W JavaScript praca z YAML zazwyczaj wiąże się z użyciem biblioteki firm trzecich, ponieważ język ten nie zawiera wbudowanego parsera dla YAML. Jedną z najpopularniejszych bibliotek do tego celu jest `js-yaml`. Możesz użyć `js-yaml`, aby przetworzyć YAML na obiekty JavaScript i odwrotnie.

Najpierw musisz zainstalować `js-yaml`:

```bash
npm install js-yaml
```

Następnie możesz jej użyć w swoich projektach. Oto jak możesz załadować plik YAML i przetworzyć go na obiekt JavaScriptowy:

```javascript
// Wymagany moduł js-yaml
const yaml = require('js-yaml');
const fs   = require('fs');

// Wczytanie YAML z pliku
try {
  const doc = yaml.load(fs.readFileSync('./config.yaml', 'utf8'));
  console.log(doc);
} catch (e) {
  console.error(e);
}
```

Jeśli twój plik `config.yaml` wygląda tak:

```yaml
version: 1
services:
  web:
    image: "myapp/web:latest"
    ports:
      - "5000:5000"
```

Wynik będzie następujący:

```javascript
{ version: 1,
  services: 
   { web: 
      { image: 'myapp/web:latest',
        ports: [ '5000:5000' ] } } }
```

Aby zrobić odwrotnie, czyli przekształcić obiekt JavaScriptowy na ciąg YAML:

```javascript
const yaml = require('js-yaml');
const obj = {
  version: 1,
  services: {
    web: {
      image: "myapp/web:latest",
      ports: ["5000:5000"]
    }
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

Ten kod wyprodukuje:

```yaml
version: 1
services:
  web:
    image: myapp/web:latest
    ports:
      - '5000:5000'
```

Używając `js-yaml`, możesz łatwo zintegrować parsowanie i serializację YAML w swoich projektach JavaScriptowych, zwiększając wymienialność danych i zarządzanie konfiguracją.
