---
title:                "Clojure: Генерування випадкових чисел."
simple_title:         "Генерування випадкових чисел."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Чому

Генерація випадкових чисел є корисною функцією в програмуванні, оскільки вона дозволяє створювати випадкові дані для тестування або симуляції. Це може бути також корисно для створення генератора випадкових паролів або випадкового вибору елементів.

## Як

Існує декілька способів генерування випадкових чисел в Clojure. Один з них - використання вбудованої функції `rand`, яка повертає випадкове дійсне число між 0 і 1 (включно):

```Clojure
(rand)
; => 0.9571914212101505
```

Щоб отримати випадкове ціле число, ми можемо використати функцію `rand-int`, яка приймає в параметрах діапазон від 0 до n-1:

```Clojure
(rand-int 10)
; => 7
```

Інший спосіб - використання модуля `java.util.Random`, який дозволяє більшу гнучкість при генеруванні випадкових чисел:

```Clojure
(require '[clojure.data.generators :as gen])

(def random-number-generator (java.util.Random.))

(gen/generate (gen/such-that even? (gen/int random-number-generator)))
; => 36
```

## Глибше

На практиці, випадкові числа не є повністю випадковими, оскільки використовуються алгоритми для їхньої генерації. Це може призвести до певної непередбачуваності, яка викликає випадковість, але деякі з цих алгоритмів можуть бути загрозливими для безпеки. У Clojure є декілька сторонніх бібліотек, які можуть бути використані для створення більш випадкових чисел, таких як `secure-random` або `crypt-random`.

## Дивіться також

- [Офіційна документація Clojure для функцій генерації випадкових чисел](https://clojuredocs.org/clojure.core/rand)
- [Стаття про генерування випадкових чисел в Clojure на сайті Medium](https://medium.com/@ashikhman/random-number-generation-in-clojure-64e386f48878)
- [Бібліотека secure-random](https://github.com/raffy3000/secure-random)
- [Бібліотека crypt-random](https://github.com/borkdude/crypt-random)