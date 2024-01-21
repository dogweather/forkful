---
title:                "Визначення довжини рядка"
date:                  2024-01-20T17:47:34.319095-07:00
model:                 gpt-4-1106-preview
simple_title:         "Визначення довжини рядка"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Що та чому?
Визначення довжини рядка — це процес отримання кількості символів у рядку. Програмісти це роблять, щоб валідувати вхід, управляти текстовими даними чи обрізати вміст під потрібний формат.

## Як це зробити:
Clojure використовує функцію `count` для отримання довжини рядка. Ось як це працює:

```Clojure
(count "Привіт, як справи?")
; => 18
```

Код вище повертає довжину рядка у символах.

## Пірнання у глибину:
В історичному контексті, функція `count` у Clojure інтуїтивно зрозуміла, тому що вона аналогічна до багатьох інших мов програмування. Але варто пам'ятати, що Clojure обробляє рядки як послідовності, тому `count` є універсальною функцією, яка може визначати довжину будь-якої послідовності, не тільки рядків.

Інші мови можуть використовувати функції типу `length` або `size`, але в Clojure справа за `count`.

Щодо реалізації, `count` працює ефективно зі строками, оскільки Clojure запозичує свої рядкові операції безпосередньо з Java, що означає, що ви використовуєте перевірені часом функції на надзвичайно популярному та надійному рушії.

## Дивись також:
- Clojure’s `count` documentation: [clojuredocs.org/clojure.core/count](https://clojuredocs.org/clojure.core/count)
- Java String length(): [docs.oracle.com](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#length())