---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:41.605910-07:00
description: "YAML, b\u0119d\u0105cy skr\xF3tem od \"YAML Ain't Markup Language\"\
  , to format serializacji danych, kt\xF3ry jest czytelny dla cz\u0142owieka. Programi\u015B\
  ci cz\u0119sto u\u017Cywaj\u0105 go do\u2026"
lastmod: '2024-03-13T22:44:35.816666-06:00'
model: gpt-4-0125-preview
summary: "YAML, b\u0119d\u0105cy skr\xF3tem od \"YAML Ain't Markup Language\", to\
  \ format serializacji danych, kt\xF3ry jest czytelny dla cz\u0142owieka. Programi\u015B\
  ci cz\u0119sto u\u017Cywaj\u0105 go do\u2026"
title: Praca z YAML
---

{{< edit_this_page >}}

## Co i dlaczego?

YAML, będący skrótem od "YAML Ain't Markup Language", to format serializacji danych, który jest czytelny dla człowieka. Programiści często używają go do plików konfiguracyjnych oraz wymiany danych pomiędzy językami ze względu na jego prostotę i czytelność w porównaniu z JSON lub XML.

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
