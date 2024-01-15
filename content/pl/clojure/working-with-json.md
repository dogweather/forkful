---
title:                "Praca z formatem json"
html_title:           "Clojure: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

JSON (JavaScript Object Notation) jest powszechnie wykorzystywanym formatem do przesyłania danych w aplikacjach internetowych. Dla osób pracujących z programowaniem w Clojure, umiejętność manipulowania danymi JSON jest niezbędna do efektywnej pracy z API i interfejsami użytkownika.

## Jak to zrobić

```Clojure
;; Tworzenie pustego obiektu JSON
(def empty-json (json/write-str {}))

;; Tworzenie obiektu JSON z danymi
(def user-json (json/write-str {:name "John" :age 30}))

;; Przerabianie danych JSON na postać Clojure
(json/read-str user-json)

;; Odczytywanie wartości z obiektu JSON
(def user-name (-> user-json json/parse-keyword :name))
(def user-age (-> user-json json/parse-keyword :age))

;; Przykładowe użycie z funkcją API
(def endpoint "https://example.com/api/users")
(def users-response (http/get endpoint)) ;; zwraca odpowiedź w formacie JSON
(json/read-str (:body users-response)) ;; przetwarza JSON na postać Clojure
```
**Wyjście:**

```Clojure
"{}" ;; empty-json
"{\"name\":\"John\",\"age\":\"30\"}" ;; user-json
{:name "John", :age "30"} ;; dane przetworzone do postaci Clojure
;; wyjście z funkcji API w formacie JSON
"{\"users\":[{\"id\":1,\"name\":\"John\"},{\"id\":2,\"name\":\"Anna\"},{\"id\":3,\"name\":\"Mike\"}]}"
```

## Głębsze zagadnienia

Ponieważ JSON jest często wykorzystywanym formatem w programowaniu w języku Clojure, istnieje wiele bibliotek i narzędzi do jego obsługi. Jednym z najpopularniejszych jest biblioteka `data.json` zawarta w standardowej bibliotece Clojure. Pozwala ona na konwersję między danymi Clojure a JSON, a także na wykonywanie zaawansowanych operacji na danych, takich jak filtrowanie, sortowanie czy grupowanie.

W przypadku bardziej zaawansowanych zastosowań, warto również zapoznać się z bibliotekami `cheshire` oraz `jsonista`, które oferują wydajniejsze funkcje przetwarzania danych JSON.

## Zobacz również

- Oficjalna dokumentacja biblioteki `data.json`: https://clojure.github.io/data.json/
- Dokumentacja biblioteki `cheshire`: https://github.com/dakrone/cheshire
- Dokumentacja biblioteki `jsonista`: https://github.com/metosin/jsonista