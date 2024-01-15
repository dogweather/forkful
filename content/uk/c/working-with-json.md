---
title:                "Робота з json"
html_title:           "C: Робота з json"
simple_title:         "Робота з json"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/working-with-json.md"
---

{{< edit_this_page >}}

"## Чому"

Робота з JSON є важливою для програмістів C, оскільки цей формат даних є одним з найпоширеніших для обміну даними між різними системами. Використовуючи C для роботи з JSON, ви можете ефективно обробляти ці дані та зберігати їх у своїх програмах.

"## Як"

Нижче показано, як працювати з JSON в C за допомогою бібліотеки JSON-C:

```C
#include <json-c/json.h>
#include <stdio.h>

int main() {

  //Створення об'єкту JSON
  json_object *object = json_object_new_object();

  //Додавання значення до об'єкту
  json_object_object_add(object, "name", json_object_new_string("John"));
  json_object_object_add(object, "age", json_object_new_int(25));

  //Отримання рядкового представлення об'єкту JSON
  const char *json_string = json_object_to_json_string(object);
  printf("JSON string: %s\n", json_string);
  
  //Отримання значення з об'єкту по ключу
  struct json_object *name;
  json_object_object_get_ex(object, "name", &name);
  printf("Name: %s\n", json_object_get_string(name));

  //Виведення значення об'єкту в форматі ключ: значення
  json_object_object_foreach(object, key, val) {
    printf("%s: %s\n", key, json_object_get_string(val));
  }

  //Звільнення пам'яті об'єкта JSON
  json_object_put(object);

  return 0;
}
```
Вихідні дані:
```
JSON string: {"name": "John", "age": 25}
Name: John
name: John
age: 25
```

"## Глибокий занурення"

Щоб працювати з більш складними структурами даних, такими як вкладені об'єкти або масиви, використовуйте функції json_object_object_get та json_object_array_get для доступу до певних значень. Також можна використовувати інші функції для перевірки наявності ключа чи індексу в об'єкті або масиві, а також для додавання чи видалення елементів.

Більш детальну інформацію про функції та їх використання можна знайти в офіційній документації бібліотеки JSON-C. 

"## Див. також"

- Офіційна документація бібліотеки JSON-C: http://json-c.github.io/json-c/
- Розширена робота з JSON в C за допомогою бібліотеки jsmn: https://spin.atomicobject.com/2014/06/24/advanced-c-libraries-json-c/
- Нативна робота з JSON в C за допомогою стандартної бібліотеки string.h: https://stackoverflow.com/questions/8303755/json-parsing-in-c-without-a-library