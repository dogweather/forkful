---
date: 2024-01-20 17:59:33.250170-07:00
description: "How to: W Clojure wysy\u0142anie \u017C\u0105dania HTTP jest \u0142\
  atwe. U\u017Cyjemy popularnej biblioteki `clj-http`. Zainstaluj j\u0105, dodaj\u0105\
  c do twojego `project.clj`."
lastmod: '2024-03-13T22:44:34.991405-06:00'
model: gpt-4-1106-preview
summary: "W Clojure wysy\u0142anie \u017C\u0105dania HTTP jest \u0142atwe."
title: "Wysy\u0142anie \u017C\u0105dania HTTP"
weight: 44
---

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
