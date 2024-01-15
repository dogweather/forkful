---
title:                "Робота з yaml"
html_title:           "C: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## Чому: 
Необхідність роботи з YAML виникає, коли треба зберегти та обробити дані у вигляді структурованого тексту, зокрема при розробці програмних продуктів. 

## Як: 
Для роботи з YAML у мові програмування C вам знадобиться саме поширений та потужний засіб - бібліотека *libyaml*. Для початку, підключіть бібліотеку у свій проект за допомогою наступного коду:
```C
#include <yaml.h>
```
Далі можна розглянути приклад запису та збереження даних у форматі YAML:
```C
// Створення карти (map) у форматі YAML
yaml_document_t map;
yaml_document_initialize(&map, NULL, NULL, NULL, 0, 0);

// Додавання пари ключ-значення у карту
yaml_node_t *key = yaml_document_add_scalar(&map, NULL, (yaml_char_t *) "name", strlen("name"), YAML_PLAIN_SCALAR_STYLE);
yaml_node_t *value = yaml_document_add_scalar(&map, NULL, (yaml_char_t *) "John", strlen("John"), YAML_PLAIN_SCALAR_STYLE);

// Запис карту до файлу
FILE *file = fopen("data.yaml", "w");
yaml_document_dump(file, &map);
fclose(file);
```

Результатом цього коду буде файл `data.yaml`, який можна прочитати за допомогою будь-якого текстового редактора. Він буде містити наступне:
```yaml
name: John
```

## Глибокі деталі: 
Якщо потрібно зберігати більше складну структуру даних, або додатково керувати виведенням та обробкою даних, бібліотека *libyaml* також надає функції для роботи з деревами та потоками у форматі YAML. Детальну інформацію про ці функції можна знайти у [документації бібліотеки](https://pyyaml.org/wiki/LibYAML).

## Дивіться також: 
- [Офіційний сайт бібліотеки *libyaml*](https://pyyaml.org/wiki/LibYAML)
- [Документація з роботи з форматом YAML у мові програмування C](https://yaml.org/spec/1.2/spec.html#id2547217)