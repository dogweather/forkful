---
date: 2024-01-20 17:43:37.385573-07:00
description: "Pobieranie strony internetowej oznacza \u015Bci\u0105gni\u0119cie jej\
  \ zawarto\u015Bci do lokalnej pami\u0119ci komputera. Programi\u015Bci to robi\u0105\
  , by przetworzy\u0107 dane, zautomatyzowa\u0107\u2026"
lastmod: '2024-03-13T22:44:34.993389-06:00'
model: gpt-4-1106-preview
summary: "Pobieranie strony internetowej oznacza \u015Bci\u0105gni\u0119cie jej zawarto\u015B\
  ci do lokalnej pami\u0119ci komputera. Programi\u015Bci to robi\u0105, by przetworzy\u0107\
  \ dane, zautomatyzowa\u0107\u2026"
title: Pobieranie strony internetowej
---

{{< edit_this_page >}}

## What & Why?
Pobieranie strony internetowej oznacza ściągnięcie jej zawartości do lokalnej pamięci komputera. Programiści to robią, by przetworzyć dane, zautomatyzować zadania lub po prostu zarchiwizować informacje.

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
