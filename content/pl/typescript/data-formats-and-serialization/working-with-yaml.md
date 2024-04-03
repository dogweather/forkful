---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:55.100865-07:00
description: "YAML, j\u0119zyk serializacji danych zaprojektowany by by\u0107 przyjazny\
  \ dla cz\u0142owieka, jest cz\u0119sto u\u017Cywany do plik\xF3w konfiguracyjnych,\
  \ komunikacji\u2026"
lastmod: '2024-03-13T22:44:35.160694-06:00'
model: gpt-4-0125-preview
summary: "YAML, j\u0119zyk serializacji danych zaprojektowany by by\u0107 przyjazny\
  \ dla cz\u0142owieka, jest cz\u0119sto u\u017Cywany do plik\xF3w konfiguracyjnych,\
  \ komunikacji mi\u0119dzyprocesowej i przechowywania danych."
title: Praca z YAML
weight: 41
---

## Jak?
Praca z YAML w TypeScript zazwyczaj obejmuje parsowanie treści YAML do obiektów JavaScript i ewentualnie konwertowanie obiektów JavaScript z powrotem na YAML. Wymaga to parsowania; popularnym wyborem jest `js-yaml`, biblioteka, która może być łatwo zintegrowana z projektami TypeScript.

### Instalowanie js-yaml
Po pierwsze, dodaj `js-yaml` do swojego projektu:

```bash
npm install js-yaml
```

### Parsowanie YAML do obiektu JavaScript
Wyobraź sobie, że masz plik YAML `config.yaml` z następującą zawartością:

```yaml
database:
  host: localhost
  port: 5432
  username: user
  password: pass
```

Możesz odczytać i sparsować ten plik do obiektu JavaScript w następujący sposób:

```typescript
import * as fs from 'fs';
import * as yaml from 'js-yaml';

// Wczytaj i sparsuj plik YAML
const fileContents = fs.readFileSync('./config.yaml', 'utf8');
const data = yaml.load(fileContents) as Record<string, any>;

console.log(data);
```

**Przykładowe wyjście:**

```json
{
  "database": {
    "host": "localhost",
    "port": 5432,
    "username": "user",
    "password": "pass"
  }
}
```

### Konwersja obiektu JavaScript do YAML
Jeśli potrzebujesz dokonać konwersji w drugą stronę i przekonwertować obiekt JavaScript na ciąg YAML, możesz użyć `js-yaml` w następujący sposób:

```typescript
import * as yaml from 'js-yaml';

const obj = {
  title: "Example",
  is_published: true,
  author: {
    name: "Jane Doe",
    age: 34
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

**Przykładowe wyjście:**

```yaml
title: Example
is_published: true
author:
  name: Jane Doe
  age: 34
```

Ten fragment kodu konwertuje obiekt JavaScript na ciąg YAML i wypisuje go. W praktyce możesz zapisać to z powrotem do pliku lub użyć w innych częściach aplikacji.
