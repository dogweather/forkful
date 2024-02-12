---
title:                "Wysyłanie żądania HTTP"
aliases: - /pl/clojure/sending-an-http-request.md
date:                  2024-01-20T17:59:33.250170-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie żądania HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
"Wysyłanie żądania HTTP to proces komunikowania się z serwerem w celu pobrania lub wysłania danych. Programiści robią to, aby wymieniać dane z aplikacjami i usługami internetowymi."

## How to:
W Clojure wysyłanie żądania HTTP jest łatwe. Użyjemy popularnej biblioteki `clj-http`. Zainstaluj ją, dodając do twojego `project.clj`:
```Clojure
[clj-http "3.12.3"]
```

Oto przykład żądania GET:
```Clojure
(require '[clj-http.client :as client])

(let [response (client/get "https://httpbin.org/get")]
  (println (:status response))
  (println (:headers response))
  (println (:body response)))
```

Odpowiedź:
```
200
{...headers...}
{
  "args": {}, 
  "headers": {
    "Host": "httpbin.org",
    ...
  }, 
  "origin": "...", 
  "url": "https://httpbin.org/get"
}
```

A tu przykład POST z danymi:
```Clojure
(let [response (client/post "https://httpbin.org/post"
                            {:form-params {:foo "bar" :baz "qux"}})]
  (println (:status response))
  (println (:body response)))
```

Odpowiedź:
```
200
{
  "args": {}, 
  "data": "", 
  "files": {}, 
  "form": {
    "foo": "bar",
    "baz": "qux"
  }, 
  "headers": {
    ...
  }, 
  "json": null, 
  "origin": "...", 
  "url": "https://httpbin.org/post"
}
```

## Deep Dive
Wysyłanie żądań HTTP nie jest nowością. Powstało wraz z HTTP w 1991 roku. Alternatywą dla `clj-http` może być `http-kit` lub niskopoziomowe `java.net.HttpURLConnection`. Wybór narzędzia zależy od potrzeb projektu: `clj-http` jest łatwy w obsłudze, `http-kit` jest szybki, a `HttpURLConnection` daje pełną kontrolę nad żądaniem.

### Implementacja w `clj-http` opiera się na bibliotece `Apache HttpComponents`. Dzięki temu obsługuje funkcje takie jak:

- Automatyczne przekierowania.
- Obsługa ciasteczek.
- Zarządzanie sesjami.

Zrozumienie, jak w Clojure obsługuje się asynchroniczność i strumienie danych, może być kluczowe przy obsługiwaniu większych odpowiedzi serwera lub wysyłaniu dużych żądań.

## See Also
- Biblioteka `clj-http`: [https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
- Dokumentacja `http-kit`: [http://www.http-kit.org](http://www.http-kit.org)
- Poradnik `java.net.HttpURLConnection`: [https://docs.oracle.com/javase/tutorial/networking/urls/readingWriting.html](https://docs.oracle.com/javase/tutorial/networking/urls/readingWriting.html)
