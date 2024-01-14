---
title:    "Clojure: Обчислення дати у майбутньому чи минулому."
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому

Обчислення дати в майбутньому або минулому може бути корисним при плануванні подій або визначенні терміну дії певного процесу. Наприклад, ви можете використовувати це для обчислення дати події на определенний термін після поточної дати або для визначення минулого терміну певного процесу.

## Як це зробити
```Clojure
(require '[clj-time.core :as time])

(time/date-minus (time/today) 10) ; показує дату 10 днів тому
(time/date-plus (time/today) 5 :hours) ; показує дату через 5 годин від поточної
(time/date-from-parts 2021 07 15) ; створює дату 15 липня 2021 року
```

## Deep Dive

У Clojure є потужна бібліотека clj-time, яка надає зручні функції для роботи з датами та часом. Ви можете використовувати функції `date-plus` та `date-minus`, щоб додавати або віднімати певну кількість часу від поточної дати. Крім того, функція `date-from-parts` дозволяє створити дату з відділенням різних частин, таких як рік, місяць та день. Більше інформації про ці та інші функції можна знайти у документації clj-time.

## Дивіться також

- [clj-time документація](https://github.com/clj-time/clj-time)
- [Офіційний сайт Clojure](https://clojure.org/)
- [Основи Clojure](https://www.tutorialspoint.com/clojure/index.htm)