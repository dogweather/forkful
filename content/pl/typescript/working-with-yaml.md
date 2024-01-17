---
title:                "Praca z YAML-em"
html_title:           "TypeScript: Praca z YAML-em"
simple_title:         "Praca z YAML-em"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## O co chodzi i po co?

Praca z YAML to nic innego jak korzystanie z języka znaczników do obsługi strukturalnych danych. Jest to szczególnie przydatne dla programistów szybko tworzących i przetwarzających większe ilości informacji. Dzięki formatowi YAML możemy łatwo przechowywać i przesyłać dane oraz struktury danych między różnymi aplikacjami.

## Jak to zrobić?

```TypeScript
const yaml = require('js-yaml');

// Przykładowy plik YAML
const file = `
- employee:
    name: John
    age: 28
- employee:
    name: Jane
    age: 24
`;

// Parsowanie pliku YAML
const data = yaml.safeLoad(file);

// Wyświetlanie danych
console.log(data);

// Wynik:
// [
//     { employee: { name: 'John', age: 28 } },
//     { employee: { name: 'Jane', age: 24 } }
// ]
```

## W szerszy kontekście

YAML został stworzony w 2001 roku i pierwotnie miał służyć jako bardziej czytelna alternatywa dla formatu XML. Dzięki swojej prostocie i elastyczności, zyskał jednak popularność wśród programistów jako format przechowywania i przesyłania danych. Alternatywami dla YAML są między innymi JSON, CSV czy XML. Implementacja obsługi YAML w TypeScript jest możliwa dzięki modułowi `js-yaml`.

## Zobacz też

- Strona YAML: https://yaml.org/
- Moduł `js-yaml` na stronie npm: https://www.npmjs.com/package/js-yaml