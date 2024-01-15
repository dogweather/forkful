---
title:                "Робота із yaml"
html_title:           "PHP: Робота із yaml"
simple_title:         "Робота із yaml"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## Чому

Існує багато форматів для збереження даних, включаючи YAML. Для розробників, які працюють з PHP, важливо знати, як працювати з YAML, адже він є простим у використанні та читабельним для людей.

## Як працювати з YAML у PHP

Для початку потрібно встановити розширення YAML для PHP. Після цього можна розпочати роботу з цим форматом даних. Незважаючи на те, що YAML дуже схожий на JSON, у ньому є кілька особливостей, які потрібно знати.

```PHP
// Читання даних з файлу YAML
$file = "example.yml";
$data = yaml_parse_file($file);

// Запис даних у файл YAML
$array = ['name' => 'John', 'age' => 25];
file_put_contents("example.yml", yaml_emit($array));
```

Як бачимо, функції у PHP для роботи з YAML є дуже простими. Застосовуючи їх, можна легко зчитувати та записувати дані у форматі YAML.

## Глибоке вивчення

Як вже згадувалося, YAML дуже схожий на JSON. Проте, у YAML є кілька особливостей, які значно полегшують роботу з ним. Наприклад, у YAML можна використовувати ключові слова, щоб позначати типи даних, такі як "string" або "integer". Також можна використовувати покажчики та анкори для посилання на раніше визначені дані.

Детальніше про синтаксис YAML можна дізнатися в [офіційній документації](https://yaml.org/spec/) та [розділі про YAML у документації PHP](https://www.php.net/manual/en/book.yaml.php).

## Дивіться також

- [Офіційна документація YAML](https://yaml.org/)
- [Офіційна документація PHP](https://www.php.net/docs.php)
- [YAML vs JSON: який формат обрати](https://www.sitepoint.com/yaml-vs-json-which-serialization-format-is-better-for-your-php-project/)
- [Чому використовувати YAML у своїх проектах](https://hackernoon.com/why-you-should-use-yaml-for-configuration-files-6wi2455)