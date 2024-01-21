---
title:                "Генерація випадкових чисел"
date:                  2024-01-20T17:48:51.587375-07:00
model:                 gpt-4-1106-preview
simple_title:         "Генерація випадкових чисел"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
Генерація випадкових чисел—це метод створення чисел, що не мають заздалегідь визначеного порядку. Програмісти використовують це для тестування, імітації, ігор, та в місцях де потрібно непередбачуваність. 

## Як це робити:
В Clojure випадкові числа можна генерувати з використанням стандартної бібліотеки. Ось як:

```clojure
; Генерація випадкового цілого числа від 0 до 100
(rand-int 100)

; Генерація випадкового дійсного числа
(rand)

; Генерація випадкового числа в інтервалі від a до b
(defn random-between [a b]
  (+ a (rand (* (- b a)))))
  
(random-between 10 20) ; приклад виклику функції
```

Приклад вивіду:
```
42   ; результат rand-int
0.85 ; результат rand
17   ; результат random-between
```

## Поглиблений розгляд
Раніше в програмуванні використовувались різні методи генерації псевдовипадкових чисел, як, наприклад, лінійний конгруентний метод. Clojure використовує Java's `java.util.Random` для `(rand)` і `(rand-int)`, що є достатньо гарним для багатьох сценаріїв. Проте, якщо потрібна криптографічна стійкість, слід використовувати `java.security.SecureRandom`, що забезпечує вищий рівень непередбачуваності.

## Дивіться також
- [ClojureDocs `rand`](https://clojuredocs.org/clojure.core/rand)
- [ClojureDocs `rand-int`](https://clojuredocs.org/clojure.core/rand-int)
- [Oracle SecureRandom documentation](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)