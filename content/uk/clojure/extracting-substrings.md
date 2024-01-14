---
title:    "Clojure: Видобування підрядків"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# Чому

Видобування підрядків є важливою інструментом для роботи з рядковими даними в Clojure. Це дозволяє нам отримувати певну частину тексту з рядка, що дає можливість ефективно працювати з великими обсягами даних.

# Як це зробити

Існує кілька способів видобування підрядків в Clojure. Розглянемо два з них.

## 1. Користувальницька функція substr

Можна створити власну функцію для видобування підрядків, використовуючи функції Clojure `subs` та `count`. Приклад коду:

```Clojure
(defn substr [s start end]
  (subs s start end))
```

Виклик функції для рядка "Привіт світе":

```Clojure
(substr "Привіт світе" 0 5)
```

Вихідний рядок:

```
"Приві"
```

## 2. Використання регулярних виразів

Іншим популярним варіантом є використання регулярних виразів. Для цього застосовуються функції `re-find` та `re-matches`. Приклад коду:

```Clojure
(re-find #"іт(.*?)" "Привіт світе")
```

Вихідний рядок:

```
"т світе"
```

# Глибока занурення

Видобування підрядків дозволяє не тільки отримувати певну частину рядка, але і виконувати більш складні операції, наприклад, видалення певної частини рядка або заміна її іншим текстом. Також, використання регулярних виразів дозволяє здійснювати пошук і видобування підрядків за заданим шаблоном.

# Дивись також

- [Офіційна документація Clojure](https://clojure.org/)
- [Відеоуроки з Clojure на YouTube](https://www.youtube.com/playlist?list=PLXqkfPMxcAQLq8zty0qAX0WtdH0T7RN90)