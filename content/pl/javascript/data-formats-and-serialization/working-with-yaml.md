---
title:                "Praca z YAML"
aliases:
- /pl/javascript/working-with-yaml/
date:                  2024-02-03T19:25:41.605910-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
