---
date: 2024-01-20 17:45:16.514698-07:00
description: "How to: Clojure u\u017Cywa funkcji `subs` do wyci\u0105gania pod\u0142\
  a\u0144cuch\xF3w. Przyjrzyjmy si\u0119 przyk\u0142adom."
lastmod: '2024-03-13T22:44:34.982968-06:00'
model: gpt-4-1106-preview
summary: "Clojure u\u017Cywa funkcji `subs` do wyci\u0105gania pod\u0142a\u0144cuch\xF3\
  w."
title: "Wycinanie pod\u0142a\u0144cuch\xF3w"
weight: 6
---

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
