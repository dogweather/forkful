---
date: 2024-01-20 17:43:37.385573-07:00
description: "How to: W Clojure do pobierania stron www u\u017Cyjemy biblioteki `clj-http`.\
  \ Przyk\u0142ad."
lastmod: '2024-03-13T22:44:34.993389-06:00'
model: gpt-4-1106-preview
summary: "W Clojure do pobierania stron www u\u017Cyjemy biblioteki `clj-http`."
title: Pobieranie strony internetowej
weight: 42
---

## How to:
W Clojure do pobierania stron www użyjemy biblioteki `clj-http`. Przykład:

```Clojure
(require '[clj-http.client :as client])

(defn download-web-page [url]
  (client/get url))

;; Użycie i przykład wyjścia
(let [response (download-web-page "https://example.com")]
  (println (:status response))
  (println (:headers response))
  (println (:body response)))
```

Spodziewaj się rezultatu w rodzaju:

```Clojure
200
{"Content-Type" "text/html; charset=UTF-8", ...}
"<!doctype html>..."
```

## Deep Dive
Pobieranie stron internetowych to nie nowość. W latach 90. robiło się to przez protokoły jak FTP czy HTTP z narzędziami typu `wget`. Dzisiaj mamy wiele języków i bibliotek ułatwiających to zadanie. W Clojure `clj-http` jest popularnym wyborem, choć alternatywy jak `http-kit` czy `aleph` też są warte uwagi. `clj-http` opiera się na `Apache HttpComponents`, przez co ma wiele funkcji i jest stabilna. Dobierając narzędzie, pamiętaj o potrzebach twojej aplikacji – czy zależy ci na prostocie, wydajności, czy może ciężko jest na asynchroniczność.

## See Also
- [clj-http na GitHub](https://github.com/dakrone/clj-http)
