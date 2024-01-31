---
title:                "Praca z yaml"
date:                  2024-01-19
simple_title:         "Praca z yaml"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
YAML to format zapisu danych, podobny do JSON, ale bardziej czytelny dla człowieka. Programiści używają go często do konfiguracji aplikacji i wdrażania infrastruktury, szczególnie w projektach związanych z DevOps i konteneryzacją.

## Jak to zrobić:
```
// Do pracy z YAML w JavaScript potrzebujemy biblioteki js-yaml.
// Instalujemy ją używając npm: npm install js-yaml

const yaml = require('js-yaml');
const fs = require('fs');

// Odczytanie pliku YAML i zamiana na obiekt JavaScript.
try {
  const doc = yaml.load(fs.readFileSync('config.yaml', 'utf8'));
  console.log(doc);
} catch (e) {
  console.error(e);
}

// Przykładowy plik 'config.yaml':
// imie: "Jan"
// praca:
//   pozycja: "Programista"
//   jezyk: "JavaScript"

// Zapis obiektu JavaScript do pliku YAML.
const dataToSave = {
  name: "Kasia",
  hobby: {
    type: "Sztuki walki",
    level: "Początkujący"
  }
};

try {
  const yamlStr = yaml.dump(dataToSave);
  fs.writeFileSync('user.yaml', yamlStr, 'utf8');
  console.log('Zapisano!');
} catch (e) {
  console.error(e);
}
```

## Deep Dive:
YAML, który oznacza "YAML Ain't Markup Language", powstał na początku XXI wieku jako łatwiejsza w ludzkiej percepcji alternatywa dla XML. Format YAML jest szeroko stosowany w aplikacjach do zarządzania konfiguracją oraz w systemach orkiestracji kontenerów, takich jak Kubernetes. YAML jest formatem niezależnym od języka i może być używany w wielu językach programowania, przy czym najpopularniejsze biblioteki do obsługi YAML w JavaScript to `js-yaml` i `yamljs`.

## Zobacz też:
- Dokumentacja `js-yaml`: https://github.com/nodeca/js-yaml
- Specyfikacja YAML: https://yaml.org/spec/1.2/spec.html
- Wprowadzenie do YAML dla początkujących: https://www.codeproject.com/Articles/1214409/Learn-YAML-in-five-minutes
- YAML w kontekście Kubernetes: https://kubernetes.io/docs/concepts/configuration/overview/#general-configuration-tips
