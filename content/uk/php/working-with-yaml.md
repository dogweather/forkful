---
title:                "Робота з yaml"
html_title:           "PHP: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/working-with-yaml.md"
---

{{< edit_this_page >}}

Що & Чому?
Робота з YAML - це процес зчитування та записування структурованої інформації у форматі YAML (YAML Ain't Markup Language). YAML є простим для читання і написання текстовим форматом, який дозволяє зберігати дані у вигляді списків, об'єктів та зв'язків між ними. Його застосовують у багатьох програмах, що працюють зі структурованими даними.

Як це зробити:
```PHP
// Завантажуємо бібліотеку для роботи з YAML
use Symfony\Component\Yaml\Yaml;

// Зчитуємо дані з файлу
$data = Yaml::parseFile('data.yml');

// Вибираємо дані зі списку
echo $data['countries'][0]['name'];
// Виводить "Україна"

// Додаємо нові дані до файлу
$newData = ['cities' => ['Kyiv', 'Lviv', 'Odesa']];
Yaml::dump($newData);
// Записує "cities: ['Kyiv', 'Lviv', 'Odesa']" у файл
```

Глибше поглянемо:
YAML був створений для того, щоб бути більш зручним форматом для зберігання та передачі даних порівняно з XML. Він використовує простий синтаксис ключ-значення, який дозволяє розташовувати дані у більш зрозумілій формі для людей. Іншими альтернативами для роботи зі структурованими даними є JSON та CSV формати.

У PHP для роботи з YAML застосовується бібліотека Symfony\Component\Yaml, яка доступна для встановлення через Composer. Бібліотека надає зручні функції для роботи з даними, зокрема зчитування і записування у формат YAML.

Дивіться також:
- Документація бібліотеки Symfony\Component\Yaml: https://symfony.com/doc/current/components/yaml.html
- Офіційна сторінка YAML: https://yaml.org/