---
title:                "Praca z yaml"
html_title:           "Clojure: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą bądź osobą pracującą w branży IT, na pewno spotkałeś się z formatem pliku YAML. Jest to popularna składnia używana do przechowywania i przesyłania danych. W tym artykule dowiesz się, dlaczego warto poznać YAML oraz jak możesz go wykorzystać w swoich projektach w języku Clojure.

## Jak to zrobić

Przedstawię teraz kod w języku Clojure, który pokaże Ci, jak wykorzystać YAML w swoich projektach. Poniżej znajdziesz przykład wykorzystania biblioteki "clj-yaml" do parsowania pliku YAML:

```Clojure
(ns my-project.core
  (:require [clj-yaml.core :as yaml]))

(defn read-yaml [file]
  (yaml/parse-file file))

;; Przykładowy plik YAML
;; country:
;;  - Poland
;;  - Germany
;;  - France
(def data (read-yaml "countries.yml"))

(println (:country data)) ;; Wynik: ["Poland" "Germany" "France"]
```

Kod ten pierwotnie odczytuje plik YAML, a następnie zwraca wynik w postaci mapy Clojure. Dzięki temu możesz wykorzystać tę mapę do dalszej pracy w swoim projekcie.

## Wszczegóły

Jeśli chcesz wiedzieć więcej o pracy z YAML w języku Clojure, warto zwrócić uwagę na kilka kluczowych aspektów:

- Jeśli chcesz dokonać zmian w pliku YAML, musisz wczytać go do mapy i następnie przekształcić z powrotem do formatu YAML.
- Jeśli potrzebujesz obsłużyć specjalne znaki, takie jak znak tabulacji lub nowa linia, możesz skorzystać z funkcji clojure.string/escape lub clojure.string/unescape.
- Możesz również wykorzystać bibliotekę "yamlclj" do obsługi plików YAML w języku Clojure. Ta biblioteka oferuje wiele przydatnych funkcji i metod do pracy z YAML, takich jak parsowanie stringów lub sprawdzanie poprawności składni.

Teraz, gdy już wiesz, jak wykorzystać YAML w języku Clojure, możesz spróbować użyć go w swoich własnych projektach!

## Zobacz również

- Oficjalna dokumentacja języka Clojure - https://clojure.org/
- Biblioteka "yamlclj" - https://github.com/alexanderkiel/yaml-clj
- Biblioteka "clj-yaml" - https://github.com/lance-p/nippy