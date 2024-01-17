---
title:                "Робота з json"
html_title:           "Kotlin: Робота з json"
simple_title:         "Робота з json"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## Що це таке й чому це потрібно?
Робота з JSON - це процес взаємодії з форматом обміну даних, який має просту структуру та використовується для обміну даними між різними програмними продуктами. Програмісти використовують JSON для збереження та передачі даних без зайвих зусиль.

## Як це зробити:
Використання JSON в Котлін дуже просте. Спочатку необхідно створити об'єкт JSON за допомогою класу JSONObject. Потім можна додавати дані до об'єкта за допомогою методу put(), наприклад:

```Kotlin 
val json = JSONObject()
json.put("name", "John")
json.put("age", 25)
```

Для отримання даних з об'єкта JSON можна використати метод get(), наприклад:

```Kotlin
val name = json.get("name")
```

Результат виконання цих прикладів коду буде наступним: ```{"name": "John", "age": 25}```

## Глибоке погруження:
JSON був розроблений японським програмістом в 2001 році і з того часу став дуже популярним у веб-розробці. Можливі альтернативи для роботи з JSON включають XML та CSV формати. Завдяки простому синтаксису та широкій підтримці, JSON зараз є одним з найбільш поширених форматів для обміну даними.

## Дивіться також:
- Офіційна документація Котлін для роботи з JSON: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-json-/
- Приклади коду для роботи з JSON у Котлін: https://www.programiz.com/kotlin-programming/json
- Курс “Робота з JSON у Котлін” на платформі Udemy: https://www.udemy.com/course/json-kotlin/