---
title:                "Javascript: Praca z YAML"
simple_title:         "Praca z YAML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

# Dlaczego warto uczyć się programowania w języku Javascript?

Język Javascript jest jednym z najpopularniejszych języków programowania na świecie – jest używany przez miliony osób każdego dnia. Jest bardzo wszechstronny, ponieważ może być wykorzystywany do tworzenia interaktywnych stron internetowych, aplikacji mobilnych, gier i wielu innych. Ale dziś skupimy się na jednym z jego najważniejszych zastosowań: pracy z YAML.

## Jak to zrobić?

Programowanie w języku Javascript jest bardzo proste. Wystarczy mieć bazową wiedzę na temat składni i funkcji języka oraz umieć korzystać z narzędzi programistycznych. Aby zacząć pracę z YAML, wystarczy zainstalować odpowiednią bibliotekę lub użyć już istniejącej w swoim projekcie. Następnie możesz zacząć tworzyć i manipulować plikami YAML za pomocą prostych funkcji języka Javascript.

Poniżej znajdziesz przykładowy kod w języku Javascript, który tworzy obiekt z danymi w formacie YAML i wypisuje go w konsoli:

```Javascript
const YAML = require('js-yaml');

const data = {
  name: 'John',
  age: 30,
  job: 'developer',
};

const yamlData = YAML.safeDump(data);

console.log(yamlData);
```

Ten mały kawałek kodu pokazuje, jak łatwo można tworzyć i manipulować danymi w formacie YAML przy użyciu języka Javascript. Dzięki temu możesz w prosty sposób przekazywać dane w swoich projektach lub tworzyć pliki konfiguracyjne dla swoich aplikacji.

## Głębsze zanurzenie

Pracując z YAML, warto poznać kilka bardziej zaawansowanych funkcji języka Javascript, które ułatwią manipulowanie i przetwarzanie danych w tym formacie. Na przykład można wykorzystać bibliotekę Lodash, aby wygodnie pracować z plikami YAML.

Poniżej przykładowy kod, który wykorzystuje tę bibliotekę do odczytania pliku YAML i wyświetlenia jego zawartości w konsoli:

```Javascript
const _ = require('lodash');
const YAML = require('js-yaml');
const fs = require('fs');

const yamlFile = fs.readFileSync('data.yaml', 'utf8');
const yamlData = YAML.safeLoad(yamlFile);

console.log(_.get(yamlData, 'name'));
```

Dzięki wykorzystaniu funkcji "_.get" z biblioteki Lodash, możesz łatwo pobierać konkretne dane z pliku YAML, niezależnie od jego struktury.

# Zobacz również

- Dokumentacja języka Javascript: https://developer.mozilla.org/pl/docs/Web/JavaScript
- Biblioteka js-yaml: https://www.npmjs.com/package/js-yaml
- Biblioteka Lodash: https://lodash.com/