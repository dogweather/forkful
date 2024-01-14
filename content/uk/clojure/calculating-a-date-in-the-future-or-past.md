---
title:                "Clojure: Обчислення дати у майбутньому або минулому."
simple_title:         "Обчислення дати у майбутньому або минулому."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому

Вчорашня дата може бути корисною для різних цілей, таких як шукання дати приїзду наступної поїздки або розрахунок досягнення певного терміну. Незалежно від мети, розрахунок дати у майбутньому або минулому може бути корисним інструментом для програмістів у Клодвр. 

## Як це зробити

Використовуючи функцію `clojure.java-time/instant`, можна створити теперішню дату та додати або відняти зазначену кількість днів, місяців або років за допомогою функції `clojure.java-time/plus/days`, `clojure.java-time/plus/months` та `clojure.java-time/plus/years` відповідно. Нижче наведений код прикладу для визначення дати в майбутньому:

```Clojure
(require '[java-time :as time])

(def today (time/instant))
(def future-date (time/plus/years today 5))
(println "Дата через 5 років: " (time/format future-date "dd/MM/yyyy"))
```

Виведення цього коду буде мати наступний результат:

```
Дата через 5 років: 06/08/2026
```

Подібно, для розрахунку дати у минулому, використовуйте відповідні функції для віднімання від теперішньої дати. Наприклад:

```Clojure
(def yesterday (time/minus/days today 1))
(println "Вчорашня дата: " (time/format yesterday "dd/MM/yyyy"))
```

Виведення коду буде мати наступний результат:

```
Вчорашня дата: 04/08/2021
```

## Глибші розгляди

Розрахунок майбутньої або минулої дати досить простий інструмент, але ця функціональність може бути використана для створення більш складних функцій для обробки дат. Наприклад, можна створити функцію, яка буде розраховувати дату прибуття або виліту для подорожуючих, використовуючи дату виїзду та кількість днів подорожі.

## Дивіться також

- [Документація Clojure для `clojure.java-time/instant`](https://clojure.github.io/java-time/);
- [Приклади використання `java-time` в Clojure](https://clojure.atlassian.net/wiki/spaces/TST/pages/70046717/Testing+new+java-time+Clojure+java+time+library).