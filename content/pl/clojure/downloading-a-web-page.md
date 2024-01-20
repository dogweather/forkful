---
title:                "Pobieranie strony internetowej"
html_title:           "C#: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co to i dlaczego?

Ściąganie strony internetowej polega na pobraniu jej zawartości dla potencjalnej analizy lub przetwarzania. Programiści robią to, aby analizować dane, wychwytywać trendy, testować wydajność witryn i wiele innych celów.


## Jak to zrobić:

```clojure
(require '[clj-http.client :as client])

(defn download-page [url]
  (let [response (client/get url)]
    (when (= 200 (:status response))
      (:body response))))

(println (download-page "https://www.example.com"))
```
Powstaje kawałek kodu w języku Clojure, który pobiera stronę internetową i wyświetla jej zawartość.

## Deep Dive:

Clojure jest nowoczesnym dialektem języka Lisp, który jest wykorzystywany w przetwarzaniu danych od 1958 roku. Inne języki, takie jak Python lub Java, także służą do ściągania stron internetowych, ale Clojure umożliwa prostą obsługę wielowątkowości i niezawodną przetwarzanie danych.

Co do szczegółów implementacji, funkcja `client/get` z biblioteki `clj-http.client` wysyła żądanie GET do wyznaczonego adresu URL, a następnie zwraca mapę odpowiedzi z wieloma polami. W naszym przypadku interesuje nas pole `:body`, które zawiera treść HTML pobranej strony.

## Zobacz też:

- Pełna dokumentacja clj-http: https://github.com/dakrone/clj-http
- Spis treści "Clojure dla początkujących": http://www.clojurebook.com
- Wprowadzenie do Clojure - obsługa sieci: https://www.braveclojure.com/web-applications/