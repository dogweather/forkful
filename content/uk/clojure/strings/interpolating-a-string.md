---
title:                "Інтерполяція рядків"
aliases:
- uk/clojure/interpolating-a-string.md
date:                  2024-01-20T17:51:12.698149-07:00
model:                 gpt-4-1106-preview
simple_title:         "Інтерполяція рядків"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Що та Чому?

Інтерполяція рядків - це техніка вставки виразів або змінних безпосередньо у рядок тексту. Програмісти використовують її, щоб зробити код чистішим та більш читабельним, уникаючи зайвої конкатенації.

## Як робити:

Clojure не має вбудованої підтримки інтерполяції рядків, але ми можемо використовувати `str` або `format` функції, або `clojure.pprint/cl-format`, яка схожа на `format` у Common Lisp.

```Clojure
; Використання `str`
(def name "Максим")
(str "Привіт, " name "!")

; Використання `format`
(format "Привіт, %s!" name)

; Використання `clojure.pprint/cl-format`
(require '[clojure.pprint :refer [cl-format]])
(cl-format nil "Привіт, ~a!" name)
```

Приклади виводу:

```
"Привіт, Максим!"
"Привіт, Максим!"
"Привіт, Максим!"
```

## Глибше занурення

Інтерполяція рядків в інших мовах, наприклад у Ruby чи JavaScript, зазвичай простіша і більш прямолінійна. Clojure робить це через функції вище, бо він приділяє більше уваги незмінності даних і функціональності.

Щодо альтернатив, іноді ліби, наприклад `clojure.string`, можуть допомогти з загальними операціями рядків, але для інтерполяції вони не так потужні.

Імплементація `clojure.pprint/cl-format` взята з Common Lisp і дуже потужна. Вона дозволяє форматувати майже будь-що, але при цьому вимагає більше часу для вивчення та розуміння параметрів форматування.

## Дивись також

- [clojuredocs.org clojure.string](https://clojuredocs.org/clojure.string)
- [clojuredocs.org clojure.pprint/cl-format](https://clojuredocs.org/clojure.pprint/cl-format)
