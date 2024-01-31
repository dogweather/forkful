---
title:                "Praca z yaml"
date:                  2024-01-19
html_title:           "Arduino: Praca z yaml"
simple_title:         "Praca z yaml"

category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
Pracując z YAML, manipulujemy danymi w formacie, który jest czytelny dla człowieka. Programiści używają YAML, bo jest prosty w użyciu i świetnie sprawdza się do konfiguracji lub wymiany danych między różnymi językami programowania.

## How to:
Clojure nie ma wbudowanego wsparcia dla YAML, więc użyjemy zewnętrznej biblioteki. Podstawowe zależności do `project.clj` to:

```clojure
[clj-yaml "0.7.0"]
[cheshire "5.10.1"]
```

Odczyt YAML:

```clojure
(require '[clj-yaml.core :as yaml])

(def data (yaml/parse-string "
- just: some example
- with:
  - multiple: values
  - and: structures"))

(println data)
```

Sample output:

```clojure
({:just "some example"} {:with [{:multiple "values"} {:and "structures"}]})
```

Zapis do YAML:

```clojure
(require '[cheshire.core :as json])

(defn to-yaml [data]
  (json/encode data))

(println (to-yaml {:hello "world"}))
```

Sample output:

```yaml
hello: "world"
```

## Deep Dive
YAML, który oznacza "YAML Ain't Markup Language," powstał w 2001. Jest alternatywą dla JSON i XML, często używa się go w aplikacjach do przechowywania konfiguracji. YAML jest bardziej zwięzły i czytelny niż XML, ma też mniej pułapek niż JSON, jak np. dodatkowe przecinki.
`clj-yaml` korzysta z biblioteki SnakeYAML dla Javy, zapewniając interoperacyjność. Cheshire pozwala przekształcać dane Clojure do formatu JSON, który potem można przetworzyć na YAML.

## See Also
- Oficjalna specyfikacja YAML: https://yaml.org/spec/1.2/spec.html
- Projekt clj-yaml na GitHub: https://github.com/clj-commons/clj-yaml
- Dokumentacja Cheshire dla JSON w Clojure: https://github.com/dakrone/cheshire
