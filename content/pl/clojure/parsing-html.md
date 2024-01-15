---
title:                "Analizowanie struktury html"
html_title:           "Clojure: Analizowanie struktury html"
simple_title:         "Analizowanie struktury html"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## Dlaczego

Każdy, kto zajmuje się programowaniem stron internetowych, musiał w pewnym momencie mierzyć się z parsowaniem HTML. Jest to proces konwertowania kodu HTML na dane, które można łatwo przetwarzać i wykorzystywać w programach. Bez tego umiejętności, trudno jest móc efektywnie pracować z treścią zamieszczoną w sieci.

## Jak to zrobić

Parsowanie HTML w języku Clojure jest niezwykle proste dzięki bibliotece Enlive. Aby zacząć, wystarczy zainstalować ją w projekcie za pomocą menedżera zależności, a następnie zaimportować do kodu:

```Clojure
(ns nazwa-projektu
  (:require [net.cgrand.enlive-html :as html]))
```

Kiedy już mamy dostęp do biblioteki, możemy użyć funkcji `parse` do przetwarzania kodu HTML i uzyskania struktury danych w postaci drzewa DOM:

```Clojure
(html/parse "<div>Hello World</div>")
```

Wyjściem z powyższego przykładu będzie kolejka zawierająca jeden element - w tym przypadku `div` z tekstem "Hello World". Możemy również użyć selektorów CSS, aby wybrać konkretne elementy z tekstu HTML:

```Clojure
(html/select (html/parse "<ul><li>Apple</li><li>Orange</li></ul>") "li")
```

Powyższy kod zwróci kolekcję z dwoma elementami - `Apple` i `Orange`. Dzięki temu, można wygodnie pobierać i przetwarzać konkretne dane z kodu HTML, bez potrzeby przeszukiwania go manualnie.

## Głębsza analiza

Parsowanie HTML może się wydawać proste, ale w rzeczywistości jest to dość skomplikowany proces. Dlatego warto zagłębić się w dokumentację biblioteki Enlive, aby poznać wszystkie możliwości, które oferuje. Można na przykład wykorzystać funkcję `at` do pobierania atrybutów elementów:

```Clojure
(html/at (html/parse "<a href="https://example.com">Link</a>") :href)
```

Wyjściem będzie wartość `https://example.com`. Inną przydatną funkcją jest `transform`, która pozwala na zmianę struktury drzewa DOM:

```Clojure
(html/transform (html/parse "<ul><li>Apple</li><li>Orange</li></ul>") [(html/select "li") html/html->text])
```

Powyzszy kod zwróci kolekcję z dwoma elementami `Apple` i `Orange` w postaci tekstu, a nie drzewa DOM. Dzięki temu, można łatwiej manipulować pobranymi danymi i dostosowywać je do potrzeb.

## Zobacz również

1. [Dokumentacja biblioteki Enlive](https://github.com/cgrand/enlive)
2. [Poradnik parsowania HTML w Clojure](https://www.braveclojure.com/functional-error-handling/)