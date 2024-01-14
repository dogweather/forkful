---
title:                "Clojure: Конкатенація рядків"
simple_title:         "Конкатенація рядків"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Чому

Процес конкатенації строк є невід'ємною частиною роботи зі строками у багатьох програмах. Використання цієї техніки дозволяє об'єднати декілька строк в одну, що зробить код більш читабельним та зменшить кількість викликів до пам'яті.

## Як це зробити

Для початку, необхідно визначити змінну, яка буде містити об'єднану строку. Далі з допомогою функції `str`, ми можемо виконати конкатенацію двох або більше строк.

```Clojure
(def first-name "Олександр")
(def last-name "Пушкін")

(def full-name (str first-name " " last-name))

(println full-name)
; Виведе "Олександр Пушкін"
```

Також можна використовувати функцію `format`, яка дозволяє об'єднувати строки зі значеннями змінних.

```Clojure
(def age 37)
(def occupation "письменник")

(def bio (format "Мене звати %s. Мені %d років і я є %s." first-name age occupation))

(println bio)
; Виведе "Мене звати Олександр. Мені 37 років і я є письменник."
```

## Глибше поглядуємо

Окрім звичайної конкатенації, Clojure також має функцію `join`, за допомогою якої можна об'єднувати елементи колекції в строку з роздільником.

```Clojure
(def fruits ["яблука" "банани" "виноград"])

(def fruit-string (join ", " fruits))

(println fruit-string)
; Виведе "яблука, банани, виноград"
```

Також в Clojure є можливість використання макросів для покращення продуктивності конкатенації строк. Один з таких макросів - `str-join`, який дозволяє замінити кілька викликів до `str` на один.

```Clojure
(defn format-list [items]
  (str-join " | " (map str items)))
  
(format-list ["пункт 1" "пункт 2" "пункт 3"])
; ВИведе "пункт 1 | пункт 2 | пункт 3"
```

## Дивіться також

- [Офіційна документація Clojure](https://clojure.org/)
- [Стаття про роботу зі строками у Clojure](https://coderwall.com/p/k1omoq/string-manipulation-functions-in-clojure)
- [Процес конкатенації строк у Java та Clojure порівняно](https://stackoverflow.com/questions/5674278/is-concatenating-strings-with-str-more-efficient-than-using-concat-in-clojure)