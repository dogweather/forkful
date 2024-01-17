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

## Що це таке і чому програмісти це роблять?

JSON є скороченням від "JavaScript Object Notation" і є форматом обміну даними, який дозволяє легко читати та писати структуровані дані. Програмісти використовують JSON для обміну даними між програмними додатками та серверами, зокрема при розробці веб-додатків. Цей формат є популярним серед програмістів за його простоту та універсальність.

## Як використовувати:

### Читання та запис JSON даних:
```C
// підключення необхідних бібліотек
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <json-c/json.h>

// створення вихідного JSON об’єкту
json_object * book = json_object_new_object();
// додавання ключів та значень до об’єкту
json_object_object_add(book, "title", json_object_new_string("У джунглях Юнголи"));
json_object_object_add(book, "author", json_object_new_string("Братья Уайт"));
json_object_object_add(book, "year", json_object_new_int(1929));
// друк у вигляді рядка
printf("Книга: %s, %s, %d.", json_object_get_string(json_object_object_get(book, "title")), json_object_get_string(json_object_object_get(book, "author")), json_object_get_int(json_object_object_get(book, "year")));

// створення нового JSON об’єкту з рядка
json_object * json_string = json_tokener_parse("{\"name\":\"Іван\", \"age\":30}");
// отримання значень за ключами
const char * name = json_object_get_string(json_object_object_get(json_string, "name"));
int age = json_object_get_int(json_object_object_get(json_string, "age"));
// друк у вигляді рядка
printf("Ім’я: %s, Вік: %d.", name, age);
```

### Створення та запис JSON об’єкту в файл:
```C
// створення об’єкту
json_object * product = json_object_new_object();
// додавання значень до об’єкту
json_object_object_add(product, "id", json_object_new_int(12345));
json_object_object_add(product, "name", json_object_new_string("Книжка"));
json_object_object_add(product, "price", json_object_new_double(19.99));
// створення JSON форматуваного рядка з об’єкту
const char * json_string = json_object_to_json_string_ext(product, JSON_C_TO_STRING_PRETTY);
// відкриття файлу для запису
FILE * file = fopen("product.json", "w");
if (file != NULL) {
	// запис у файл
	fputs(json_string, file);
	printf("Файл успішно записано.");
} else {
	printf("Помилка запису у файл.");
}
fclose(file);
```

## Поглиблене вивчення:

### Історичний контекст:
JSON був вперше використаний в 2001 році Дугом Крокфордом для обміну даними в мові JavaScript. Згодом, цей формат став стандартом для обміну даними між програмними додатками та веб-сайтами.

### Альтернативи:
Існують різні альтернативи для обміну даними, такі як XML, YAML та CSV. Однак, JSON є більш легким та зрозумілим для програмістів, що робить його більш популярним у веб-розробці.

### Деталі реалізації:
Бібліотека JSON-C є однією з популярних для роботи з JSON у мові C. Вона дозволяє створювати та перетворювати об’єкти JSON, а також читати та записувати дані із файлів. Для отримання більш детальної інформації про реалізацію JSON у C можна ознайомитись з документацією бібліотеки.

## Також подивіться:

- [Документація по бібліотеці JSON-C](https://github.com/json-c/json-c/wiki)
- [JSON офіційний сайт](https://www.json.org/json-en.html)