---
title:                "C: Робота з json"
simple_title:         "Робота з json"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/working-with-json.md"
---

{{< edit_this_page >}}

## Чому

JSON є одним з найпопулярніших форматів обміну даними в програмуванні, особливо веброзробки. Він є простим для розуміння і використання, а також підтримується більшістю мов програмування. Знання роботи з JSON дозволить вам легко обмінюватися даними між різними додатками і платформами.

## Як

Використання JSON в мові програмування C досить просте, оскільки існує кілька бібліотек, які допоможуть зробити роботу з ним ще простішою. Нижче наведені декілька прикладів коду, які розкриють основні операції з JSON.

 ```C
#include <stdio.h>
#include <stdlib.h>
#include <jansson.h>

int main() {
    // Створення об'єкта JSON
    json_t *root = json_object();
    
    // Додавання полів
    json_object_set_new(root, "name", json_string("John"));
    json_object_set_new(root, "age", json_integer(25));
    
    // Конвертація в рядок
    char *json_str = json_dumps(root, JSON_INDENT(2));
    
    // Виведення результату
    printf("%s\n", json_str);
    
    // Звільнення пам'яті
    free(json_str);
    json_decref(root);
    
    return 0;
}
```

Вивід:

```json
{
  "name": "John",
  "age": 25
}
```

## Deep Dive

Всі дані в JSON представлені у вигляді об'єктів, масивів, рядків, чисел, булевих значень та значення null. Щоб отримати доступ до конкретного поля, можна використовувати функцію `json_object_get()`, а для зчитування значення - відповідні функції, наприклад, `json_string_value()` або `json_integer_value()`. Для отримання усіх ключів або індексів з об'єкта чи масиву можна використовувати функцію `json_object_iter()`, яка поверне ітератор на елементи.

Крім того, JSON має простий формат, що дозволяє легко читати і створювати дані вручну, якщо потрібно. Це робить його корисним інструментом для збереження і передачі даних в різних сценаріях.

## Дивіться також

- [Офіційна документація бібліотеки jansson](https://jansson.readthedocs.io)
- [Вступ до роботи з JSON в мові програмування C](https://www.cprogramming.com/tutorial/cjson.html)
- [Приклади коду з використанням бібліотеки jansson](https://github.com/akheron/jansson/tree/master/example)