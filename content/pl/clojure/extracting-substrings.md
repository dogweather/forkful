---
title:                "Wycinanie podłańcuchów"
date:                  2024-01-20T17:45:16.514698-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wycinanie podłańcuchów"

category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Wyciąganie podłańcuchów to jak wybieranie najlepszych kawałków ciasta - bierzesz tylko to, co potrzebujesz. Programiści to robią, aby operować na konkretnych fragmentach tekstów: weryfikować dane, wydobywać informacje lub po prostu dostosowywać wyświetlane wiadomości.

## How to:
Clojure używa funkcji `subs` do wyciągania podłańcuchów. Przyjrzyjmy się przykładom:

```Clojure
; Prosty przykład wyciągania części stringa
(subs "Clojure jest świetny" 8 14)
; Wynik: "jest św"

; Wyciągnięcie końca stringa bez określania końcowego indeksu
(subs "Elegancki i wydajny" 11)
; Wynik: "wydajny"

; Zagnieżdżone wyciąganie podłańcuchów
(subs (subs "Klonowy Lisp" 0 6) 0 5)
; Wynik: "Klono"
```

## Deep Dive
Clojure, będący dialektem LISP, ma swoje korzenie w przetwarzaniu list i symboli, ale świetnie radzi sobie z tekstami. Wyciąganie podłańcuchów w Clojure jest proste i bezpośrednie, podobnie jak w większości języków programowania. `subs` jest częścią rdzenia Clojure i wykorzystuje indeksy Javy String, więc działanie jest szybkie i efektywne. Alternatywy? Można użyć wyrażeń regularnych (`re-find`, `re-matches`), ale to już cięższe działa dla prostszego zadania.

## See Also
- [Clojure from the ground up: strings](https://aphyr.com/posts/305-clojure-from-the-ground-up-strings)
