---
title:                "Wysyłanie żądania http"
html_title:           "Arduino: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysyłanie żądań HTTP to proces komunikacji z serwerem, pozwalający na przesyłanie danych. Programiści robią to, aby interaktywnie komunikować się z aplikacjami internetowymi i manipulować ich danymi.

## Jak to zrobić:

Możemy wysłać żądanie HTTP w Clojure za pomocą biblioteki `clj-http`. Oto prosty przykład:

```Clojure
(ns moja-aplikacja.core
  (:require [clj-http.client :as client]))

(defn wyslij-zadanie []
  (let [odpowiedz (client/get "http://example.com")]
    (println "Status: " (:status odpowiedz))
    (println "Ciało odpowiedzi: " (:body odpowiedz))))
```

Po uruchomieniu tego kodu otrzymasz odpowiedź od `http://example.com`, a status i ciało odpowiedzi zostaną wydrukowane.

## Dogłębna analiza:

Historia HTTP zaczyna się od powstania internetu. Był to protokół wykorzystywany do komunikacji między klientem a serwerem. W Clojure, zazwyczaj korzystamy z biblioteki `clj-http` do wysyłania żądań HTTP, ale istnieją również inne alternatywy, takie jak `http-kit` czy `aleph`. Różnią się one szczegółami implementacji i oferowanymi funkcjami.

Co więcej, operacje HTTP w Clojure, tak jak w powyższym przykładzie, są blokujące. Oznacza to, że cały wątek zostaje zatrzymany, dopóki odpowiedź nie zostanie otrzymana. W praktyce, lepiej jest używać odpowiednich mechanizmów do obsługi operacji asynchronicznych.

## Zobacz też:

Więcej na temat żądań HTTP i używanych do tego bibliotek w Clojure można znaleźć na następujących stronach:
- Dokumentacja clj-http: https://github.com/dakrone/clj-http
- Przewodnik po http-kit: http://www.http-kit.org/
- Dokumentacja Aleph: https://github.com/ztellman/aleph