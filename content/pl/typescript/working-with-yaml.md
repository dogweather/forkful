---
title:                "Praca z yaml"
date:                  2024-01-19
simple_title:         "Praca z yaml"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
YAML to format służący do reprezentacji danych, ceniony za czytelność. Programiści używają YAML-a głównie do konfiguracji aplikacji, bo to łatwiejsze w utrzymaniu niż tradycyjne pliki konfiguracyjne.

## How to: (Jak to zrobić?)
```typescript
import * as yaml from 'js-yaml';
import fs from 'fs';

// Odczytanie YAML z pliku
const doc = yaml.load(fs.readFileSync('./config.yaml', 'utf8'));
console.log(doc);

// Zapisanie obiektu jako YAML do pliku
const data = { name: "Jan Kowalski", age: 30 };
fs.writeFileSync('./user.yaml', yaml.dump(data));

// Output po odczycie (załóżmy, że config.yaml zawiera `name: "Jan Kowalski"`)
// { name: 'Jan Kowalski' }

// Sprawdzanie wyników zapisu w user.yaml
// name: Jan Kowalski
// age: 30
```

## Deep Dive (Dogłębna Analiza)
YAML wypłynął w 2001 roku, szybko stając się popularnym wyborem. Alternatywą jest JSON - szybszy w przetwarzaniu, ale mniej czytelny dla ludzi. Przy pracy z YAML-em istotne są wcięcia, które definiują strukturę, a także typy danych, jak listy i mapy. W TypeScript używamy biblioteki `js-yaml` do manipulacji YAML-em, która zapewnia nam bezpieczeństwo typów i łatwość użycia.

## See Also (Zobacz Również)
- Specyfikacja YAML: https://yaml.org/spec/1.2/spec.html
- Repozytorium `js-yaml` na GitHub: https://github.com/nodeca/js-yaml
- Dokumentacja TypeScript: https://www.typescriptlang.org/docs/
- Porównanie JSON i YAML: https://phoenixnap.com/kb/yaml-vs-json
