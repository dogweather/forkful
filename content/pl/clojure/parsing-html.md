---
title:                "Przetwarzanie HTML"
date:                  2024-01-20T15:30:58.339960-07:00
html_title:           "Bash: Przetwarzanie HTML"
simple_title:         "Przetwarzanie HTML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why - Co i Dlaczego?
Parsing HTML to proces ekstrakcji danych ze struktur dokumentu HTML. Programiści robią to, aby manipulować zawartością, wydobywać informacje i integrować aplikacje z webowymi interfejsami użytkownika.

## How to - Jak to zrobić?
W Clojure do parsowania HTML używamy biblioteki [enlive](https://github.com/cgrand/enlive). Oto prosty przykład:

```clojure
(require '[net.cgrand.enlive-html :as html])

(defn parse-html [html-content]
  (html/select (html/html-resource (java.io.StringReader. html-content))
               [:div.example-class]))

(let [html-str "<div class='example-class'>Witaj, Clojure!</div>"]
  (println (parse-html html-str)))
```

Output:
```
([{:tag :div, :attrs {:class "example-class"}, :content ["Witaj, Clojure!"]}])
```

## Deep Dive - W Głębię
Parsing HTML w Clojure via `enlive` jest mocny w historycznym kontekście. Przełomowy, bo łączy deklaratywność z programowaniem funkcyjnym. Alternatywą jest użycie `jsoup` za pośrednictwem interopu Java. Jednakże, `enlive` jest "idiomatic Clojure", wykorzystuje sekwencyjne przetwarzanie - co jest naturalne dla tego języka.

Enlive rozdziela proces parsowania i manipulacji. Piersz parsujesz HTML, potem definiujesz "transformacje" do aplikowania na strukturę DOM. To częściowa odwrotność typowego podejścia, gdzie modyfikujesz DOM w locie.

## See Also - Zobacz Również
- Enlive Tutorial: https://github.com/swannodette/enlive-tutorial
- Oficjalna dokumentacja `enlive`: https://github.com/cgrand/enlive
- `jsoup`: https://jsoup.org