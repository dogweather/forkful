---
date: 2024-01-20 17:45:40.070570-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : ."
lastmod: '2024-03-13T22:44:48.634473-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0412\u0438\u0434\u0456\u043B\u0435\u043D\u043D\u044F \u043F\u0456\u0434\u0440\
  \u044F\u0434\u043A\u0456\u0432"
weight: 6
---

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
