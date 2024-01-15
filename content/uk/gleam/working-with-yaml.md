---
title:                "Співпраця з YAML"
html_title:           "Gleam: Співпраця з YAML"
simple_title:         "Співпраця з YAML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## Чому

Переваги використання YAML для зберігання та передачі даних.

## Як

Щоб почати роботу з YAML у Gleam виконайте наступні кроки:

1. Встановіть пакет `gleam-yaml` за допомогою Gleam CLI або збирача пакетів.
2. Імпортуйте `yaml` модуль у свій код.
3. Використовуйте функції `yaml.encode` та `yaml.decode` для передачі даних у форматі YAML або для отримання даних з YAML відповідно.

```Gleam
import yaml

let data = { "name": "Maria", "age": 25, "hobbies": ["reading", "hiking", "painting"] }

// Кодуємо в YAML
let encoded = yaml.encode(data)
// Результат: "name: Maria\nage: 25\nhobbies: \n- reading\n- hiking\n- painting\n"

// Декодуємо з YAML
let decoded = yaml.decode(encoded)
// Результат: data = { "name": "Maria", "age": 25, "hobbies": ["reading", "hiking", "painting"] }
```

## Deep Dive

YAML є читабельним форматом, який можна легко читати та редагувати як людям, так і програмам. Він використовує значення ключів для структурування даних та підтримує різноманітні типи даних, включаючи рядки, числа, булеві значення та масиви. Крім того, YAML підтримує коментарі, що робить його корисним для використання у конфігураційних файлах.

## Дивися також

- [Gleam YAML GitHub репозиторій](https://github.com/gleam-lang/gleam-yaml)
- [YAML документація](https://yaml.org/)
- [Gleam документація](https://gleam.run/)