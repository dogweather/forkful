---
title:                "Łączenie ciągów znaków"
html_title:           "Clojure: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Kontaktowanie lub łączenie ciągów znaków to podstawowa operacja w programowaniu, polegająca na połączeniu dwóch lub więcej ciągów w jeden. Programiści często stosują tę operację, aby tworzyć wygodne i czytelne wyjście, np. wyświetlać informacje użytkownikom.

## Jak to zrobić:

```Clojure
(str "Hello" " " "World")               ; Output: "Hello World"
(str "I have " 5 " apples")             ; Output: "I have 5 apples"
(.concat "Hey" " there")                ; Output: "Hey there"
```

## Głębszy wgląd:

Kontaktowanie ciągów znaków jest powszechnym zabiegiem w programowaniu od lat. Inną metodą łączenia ciągów jest użycie funkcji `StringBuilder` lub `StringBuffer`, które są bardziej wydajne niż standardowa konkatenacja. W Clojure możemy również użyć makra `str`, które przyjmuje wiele argumentów i łączy je w jeden ciąg.

## Zobacz również:

Więcej informacji o użyciu `str` w Clojure: https://clojuredocs.org/clojure.core/str

Inne sposoby na łączenie ciągów znaków w Clojure: https://medium.com/swl-blog/string-concatenation-in-clojure-f5609f54b1d8