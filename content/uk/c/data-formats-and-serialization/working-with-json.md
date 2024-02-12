---
title:                "Робота з JSON"
date:                  2024-02-03T18:12:35.542479-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Робота з JSON (JavaScript Object Notation) в C включає парсинг, генерацію та маніпулювання структурами даних JSON. Програмісти роблять це, щоб забезпечити комунікацію з веб-сервісами, зберігання даних або файлами конфігурації в легко читабельному форматі.

## Як це зробити:

Для роботи з JSON в C ви зазвичай будете використовувати бібліотеку, таку як `jansson` або `json-c`, через брак вбудованої підтримки JSON в C. Тут ми зосередимось на `jansson` через її простоту використання та активне супроводження. Спочатку інсталюйте бібліотеку (наприклад, використовуючи менеджер пакетів, як `apt` на Ubuntu: `sudo apt-get install libjansson-dev`).

Почнемо з парсингу JSON рядка та доступу до його вмісту:

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    const char *json_string = "{\"name\":\"John Doe\",\"age\":30}";
    json_error_t error;
    json_t *root = json_loads(json_string, 0, &error);
    
    if(!root) {
        fprintf(stderr, "error: on line %d: %s\n", error.line, error.text);
        return 1;
    }
    
    const char *name;
    int age;
    json_unpack(root, "{s:s, s:i}", "name", &name, "age", &age);
    
    printf("Ім'я: %s\nВік: %d\n", name, age);
    
    json_decref(root);
    return 0;
}
```

Приклад виводу:
```
Ім'я: John Doe
Вік: 30
```

Далі, створення та виведення JSON об'єкта:

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    json_t *root = json_object();
    json_object_set_new(root, "name", json_string("Jane Doe"));
    json_object_set_new(root, "age", json_integer(25));
    
    char *json_dump = json_dumps(root, JSON_ENCODE_ANY);
    printf("%s\n", json_dump);
    
    free(json_dump);
    json_decref(root);
    return 0;
}
```

Приклад виводу:
```
{"name": "Jane Doe", "age": 25}
```

Ці приклади демонструють основи завантаження JSON рядка, розпакування його значень, створення нового JSON об'єкта та потім його виведення у вигляді рядка.

## Поглиблено

Потреба працювати з JSON у C виникає з прийняття вебом JSON як основного формату обміну даними. Простота та ефективність JSON швидко дозволили йому випередити XML, незважаючи на початкову відсутність C у прямій підтримці маніпуляцій з JSON. Ранні рішення включали в себе ручну маніпуляцію рядками - схильну до помилок і неефективну. Бібліотеки, такі як `jansson` та `json-c`, з'явилися, щоб заповнити цей пробіл, надаючи міцні API для парсингу JSON, його створення та серіалізації.

Хоча `jansson` пропонує простоту та легкість використання, `json-c` може зацікавити тих, хто шукає більший набір функцій. Тим не менш, альтернативи, такі як бібліотеки парсингу в C++, пропонують більш складні абстракції завдяки складнішим структурам даних цієї мови та підтримці стандартної бібліотеки. Однак, коли працюють в середовищах, де C є переважною або необхідною мовою - наприклад, в системах на мікроконтролерах або при інтерфейсі з існуючими бібліотеками C - використання `jansson` або `json-c` стає незамінним.

Також варто вказати, що робота з JSON у C вимагає глибшого розуміння управління пам'яттю, оскільки ці бібліотеки часто повертають динамічно розподілені об'єкти, які вимагають явного звільнення. Це викликає у програмістів виклик знайти баланс між зручністю та відповідальністю уникнення витоків пам'яті, критично важливого аспекту створення ефективного коду на C.