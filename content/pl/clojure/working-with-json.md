---
title:                "Praca z JSON"
date:                  2024-01-19
html_title:           "Bash: Praca z JSON"
simple_title:         "Praca z JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
Praca z JSON to zarządzanie danymi w formacie, który łatwo się czyta i pisze dla ludzi oraz jest maszynowo przetwarzalny. Programiści używają JSON do wymiany danych między serwerami i aplikacjami, przechowywania konfiguracji, ustawień i stanu aplikacji.

## How to:
Clojure używa biblioteki Cheshire do pracy z JSON. Oto jak to zrobić:

```Clojure
;; Dodaj zależność do projektu
[cheshire "5.10.1"]

;; Zaimportuj bibliotekę
(require '[cheshire.core :as json])

;; Parsowanie JSON do mapy Clojure
(def json-str "{\"name\":\"Kotek\",\"likes\":[\"mleko\",\"drapanie\"]}")
(def clojure-map (json/parse-string json-str))
;; => {"name" "Kotek", "likes" ["mleko" "drapanie"]}

;; Kodowanie mapy Clojure do ciągu JSON
(def clojure-data {"type" "Kot", "age" 3})
(def json-output (json/generate-string clojure-data))
;; => "{\"type\":\"Kot\",\"age\":3}"
```

## Deep Dive
JSON, skrót od JavaScript Object Notation, zyskał popularność na początku lat 2000 jako alternatywa dla XML. Cheshire to powszechnie stosowana biblioteka Clojure do JSON, ale istnieją też inne, jak `data.json`. Wartość Cheshire to szybkość działania dzięki wykorzystaniu Jackson, niskopoziomowej biblioteki Javy do obsługi JSON.

## See Also
- Oficjalna strona JSON: [json.org](https://json.org)
- Repozytorium GitHub biblioteki Cheshire: [Cheshire on GitHub](https://github.com/dakrone/cheshire)
- Dokumentacja Clojure: [clojure.org](https://clojure.org)
