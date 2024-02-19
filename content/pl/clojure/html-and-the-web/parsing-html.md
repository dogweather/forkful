---
aliases:
- /pl/clojure/parsing-html/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:50.248374-07:00
description: "Parsowanie HTML w Clojure wi\u0105\u017Ce si\u0119 z programowym ekstrahowaniem\
  \ informacji z dokument\xF3w HTML. Programi\u015Bci robi\u0105 to, aby uzyska\u0107\
  \ dost\u0119p, manipulowa\u0107 lub\u2026"
lastmod: 2024-02-18 23:08:49.262650
model: gpt-4-0125-preview
summary: "Parsowanie HTML w Clojure wi\u0105\u017Ce si\u0119 z programowym ekstrahowaniem\
  \ informacji z dokument\xF3w HTML. Programi\u015Bci robi\u0105 to, aby uzyska\u0107\
  \ dost\u0119p, manipulowa\u0107 lub\u2026"
title: "Analiza sk\u0142adniowa HTML"
---

{{< edit_this_page >}}

## Co i dlaczego?

Parsowanie HTML w Clojure wiąże się z programowym ekstrahowaniem informacji z dokumentów HTML. Programiści robią to, aby uzyskać dostęp, manipulować lub monitorować zawartość sieci dynamicznie, automatyzując zadania lub dostarczając dane do aplikacji.

## Jak to zrobić:

Clojure nie posiada wbudowanych możliwości parsowania HTML, ale możesz użyć bibliotek Java lub wrapperów Clojure, takich jak `enlive` lub `hickory`. Oto jak używać obu:

### Używając Enlive:

Enlive to popularny wybór do parsowania HTML i skrobania stron internetowych. Najpierw dołącz go do zależności swojego projektu:

```clojure
[net.cgrand/enlive "1.1.6"]
```

Następnie możesz parsować i nawigować po HTML w następujący sposób:

```clojure
(require '[net.cgrand.enlive-html :as html])

(let [doc (html/html-resource (java.net.URL. "http://example.com"))]
  (html/select doc [:div.some-class]))
```

Ten fragment kodu pobiera stronę HTML i wybiera wszystkie elementy `<div>` z klasą `some-class`.

Wynik może wyglądać tak:

```clojure
({:tag :div, :attrs {:class "some-class"}, :content ["Oto jakaś treść."]})
```

### Używając Hickory:

Hickory zapewnia sposób na parsowanie HTML do formatu, który jest łatwiejszy do pracy w Clojure. Dodaj Hickory do zależności swojego projektu:

```clojure
[hickory "0.7.1"]
```

Oto prosty przykład:

```clojure
(require '[hickory.core :as hickory]
         '[hickory.select :as select])

;; Parsuj HTML do formatu Hickory
(let [doc (hickory/parse "<html><body><div id='main'>Witaj, świecie!</div></body></html>")]
  ;; Wybierz div z id 'main'
  (select/select (select/id "main") doc))
```

Ten kod parsuje prosty ciąg HTML i używa selektora CSS, aby znaleźć `div` z ID `main`.

Przykładowy wynik:

```clojure
[{:type :element, :tag :div, :attrs {:id "main"}, :content ["Witaj, świecie!"]}]
```

Zarówno `enlive`, jak i `hickory` oferują solidne rozwiązania do parsowania HTML w Clojure, przy czym `enlive` skupia się bardziej na szablonach, a `hickory` podkreśla transformację danych.
