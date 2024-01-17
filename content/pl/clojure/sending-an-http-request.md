---
title:                "Wysyłanie żądania http."
html_title:           "Clojure: Wysyłanie żądania http."
simple_title:         "Wysyłanie żądania http."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wysyłanie żądania HTTP jest jedną z podstawowych operacji w programowaniu. Polega ona na wysłaniu zapytania do serwera, aby pobrać informacje lub wykonac konkretne działania. Programiści korzystają z tej metody, aby pobierać i przetwarzać dane z różnych źródeł internetowych.

## Jak zrobić: 
W celu wysłania żądania HTTP w Clojure, musisz użyć funkcji "clojure.core/http-client". Możesz wykorzystać różne opcje, takie jak metoda żądania, nagłówki i ciało zapytania. Na przykład:

```Clojure
(clojure.core/http-client
  {:method :get
  :url "https://www.example.com"
  :headers {"Content-Type" "application/json"}
  :body "{:name \"John\", :age 30}"})
```
W rezultacie otrzymasz odpowiedź od serwera w postaci danych w formacie JSON.

## Głębsze zagadnienia:
Wysyłanie żądania HTTP jest powszechnie stosowane w aplikacjach internetowych i programowaniu na backendzie. Alternatywnymi sposobami na wymianę danych z serwerem są między innymi protokoły FTP i SSH. W Clojure możliwe jest również użycie biblioteki "clj-http" dla bardziej zaawansowanej obsługi żądań HTTP.

## Zobacz także: 
Więcej informacji na temat wysyłania żądania HTTP w języku Clojure można znaleźć w dokumentacji na [stronie oficjalnej](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/http-client). Warto także zapoznać się z przykładami wykorzystania tej funkcji w [bibliotece Ring](https://github.com/ring-clojure/ring/wiki/Getting-Started).