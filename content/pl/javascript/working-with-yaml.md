---
title:                "Praca z yaml"
html_title:           "Javascript: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Pracowanie z YAML to metoda konwertowania danych strukturalnych do łatwego do czytania formatu. Programiści wykorzystują to do łatwiejszego przechowywania i przetwarzania danych w plikach tekstowych.

## Jak:

Aby zacząć korzystać z YAML, wystarczy zainstalować odpowiednie moduły za pomocą menedżera pakietów i wywołać komendy w dowolnym pliku Javascript. Przykładowy kod wygląda tak:

```JavaScript
const YAML = require('yaml');
const fs = require('fs');

// zapis do pliku YAML
fs.writeFileSync('plik.yml', YAML.stringify({a: [1, 2, 3]}));

// odczyt z pliku YAML
let dane = YAML.parse(fs.readFileSync('plik.yml', 'utf8'));

console.log(dane);
// wyjście: {a: [1, 2, 3]}
```

## Głębsza zajawka:

Kilka lat temu YAML zyskał popularność dzięki swojej czytelności i elastyczności w porównaniu do innych formatów danych, takich jak XML czy JSON. Alternatywami do YAML są np. TOML i INI, ale każdy z nich ma swoją specyfikę i wykorzystanie. Implementacja YAML jest również dostępna dla wielu innych języków programowania.

## Zobacz też:

- [Dokumentacja YAML](https://yaml.org/)
- [Porównanie formatów danych](https://www.codeproject.com/Reference/720512/List-of-JSON-XML-and-YAML-Editors-Converters-and-vi)
- [Pełna lista modułów YAML w npm](https://www.npmjs.com/search?q=yaml)