---
title:                "Analiza składniowa HTML"
html_title:           "Gleam: Analiza składniowa HTML"
simple_title:         "Analiza składniowa HTML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Parsowanie HTML to proces analizowania struktury kodu HTML. Programiści robią to zarówno do ekstrakcji danych, jak i do modyfikacji zawartości stron internetowych.

## Jak to zrobić?
Użyjemy biblioteki o nazwie `Enlive`. Aby ją zainstalować, dodaj do `project.clj`: ```Clojure [net.cgrand/enlive "1.1.6"] ```. To zrozumieć, jak ją używać, przejrzymy kod:

```Clojure
(ns example.core
 (:require [net.cgrand.enlive-html :as html]))

(defn fetch-html [url]
  (html/html-resource (java.net.URL. url)))

(defn get-headings [html]
  (map :content (html/select html [:h1 :h2 :h3])))

(defn print-headings [url]
  (-> url
      fetch-html
      get-headings
      println))
```
Przykładowe wyjście:
```Clojure
("Jak to zrobic?" "Glebokie zanurzenie" "Zobacz także")
```
## Głębsze zanurzenie
Parsowanie HTML pojawiło się z narodzinami internetu, z potrzebą analizowania i modyfikowania stron internetowych. Rozważane są różne metody parsowania, w tym DOM, SAX, Pull i Stream. `Enlive` działa na zasadzie wybrania wcześniej zdefiniowanych elementów ze struktury DOM.

Rozważaj także inne biblioteki, takie jak `Jsoup` lub `HTMLUnit`, niektóre mogą lepiej pasować do twoich potrzeb. Przykładowo, `Jsoup` jest dobrą opcją, jeśli chcesz filtrować i manipulować danymi HTML. `HTMLUnit`, z drugiej strony, jest dobrym wyborem, jeśli potrzebujesz obsługi interakcji użytkownika, takiej jak kliknięcia i przewijanie.

## Zobacz także
Pomocne źródła:
- `Enlive`: [link](https://github.com/cgrand/enlive)
- `Jsoup`: [link](https://jsoup.org/)
- `HTMLUnit`: [link](http://htmlunit.sourceforge.net/)