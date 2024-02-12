---
title:                "Виділення підрядків"
aliases:
- /uk/clojure/extracting-substrings/
date:                  2024-01-20T17:45:40.070570-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виділення підрядків"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що це таке та чому?
Витягування підстрок - це процес отримання частини рядка. Програмісти використовують його для аналізу, обробки тексту, та маніпуляції даними.

## Як це зробити:
```Clojure
; Створюємо рядок
(def example-string "Привіт, Clojure українці!")

; Витягуючи підстроку використовуючи `subs`
(def substring (subs example-string 0 6))
println(substring) ; Виводить: "Привіт"

; Витягуючи слово "Clojure" з рядка
(def clojure-substr (subs example-string 8 15))
println(clojure-substr) ; Виводить: "Clojure"
```

## Занурення в глибину:
Clojure, як і багато Lisp-подібних мов, займає особливе місце в серцях програмістів через свою стриманість та ефективність. Витягування підстрок у Clojure здійснюється за допомогою функції `subs`, яка ефективно працює з умовно нескінченними "lazy" послідовностями. Головна альтернатива - використання регулярних виразів за допомогою `re-find` і `re-seq`, але це може бути надмірним для простого витягування підстрок. Історично, гнучкість стрічок в Lisp стимулювала створення потужних текстових процесорів. Ця традиція жива й до сьогодні в середовищі Clojure.

## Дивіться також:
- [Clojure Documentation on `subs`](https://clojuredocs.org/clojure.core/subs)
- [Clojure from the ground up: strings](https://aphyr.com/posts/305-clojure-from-the-ground-up-strings)
