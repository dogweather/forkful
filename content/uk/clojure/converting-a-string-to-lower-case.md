---
title:                "Перетворення рядка в нижній регістр"
html_title:           "Javascript: Перетворення рядка в нижній регістр"
simple_title:         "Перетворення рядка в нижній регістр"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що і для чого? 

Перетворення рядка на нижній регістр - це процес зміни всіх великих букв символів на маленькі букви символи в рядку. Програмісти роблять це для уніфікації даних та зручного проведення порівнянь і сортування рядків.

## Як це зробити:

В Clojure ми можемо використовувати функцію `clojure.string/lower-case` для перетворення рядків на нижній регістр:

```clojure
(require '[clojure.string :as str])

(str/lower-case "Hello World!")
```

На виході отримуємо:

```clojure
"hello world!"
```

## Занурення в деталі:

1. Контекст: Функція `lower-case` в Clojure розроблена на основі аналогічних функцій в інших мовах програмування, таких як C та Java, з метою забезпечити зручний та продуктивний інструмент для роботи з рядками.

2. Альтернативи: Існують інші способи перетворення рядків на нижній регістр в Clojure, наприклад, за допомогою функциї `map` і `Character/toLowerCase`:

```clojure
(require '[clojure.string :as str])

(apply str (map #(Character/toLowerCase %) "Hello World!"))
```

3. Деталі реалізації: Внутрішньо, `clojure.string/lower-case` використовує метод `toLowerCase` з Java String Class. Цей метод використовує налаштування локалізації за замовчуванням для перетворення символів на нижній регістр. 

## Дивіться також:

Інформацію про роботу з рядками в Clojure можна знайти в [офіційній документації](https://clojuredocs.org/clojure.string/lower-case). Для більш глибокого розуміння можна ознайомитися з існуючими [учбовим матеріалами](https://www.braveclojure.com/) та [прикладами коду](https://gist.github.com/ghoseb/2897711).