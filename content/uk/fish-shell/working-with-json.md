---
title:                "Робота з JSON"
html_title:           "Arduino: Робота з JSON"
simple_title:         "Робота з JSON"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Робота з JSON (JavaScript Object Notation) полягає у формуванні та обробці даних у легкочитаному текстовому форматі. Програмісти використовують JSON для обміну даними між сервером і клієнтом, а також для зберігання конфігурацій.

## Як це робити:
```Fish Shell
# читання JSON файлу
set json_data (cat config.json | jq '.')

# отримання значення за ключем
set user_name (echo $json_data | jq -r '.user.name')

# вивід результату
echo $user_name
# OUTPUT: Влад
```

## Поглиблення
JSON виникає в 2000-х, як відповідь на потребу в легкій і зрозумілій альтернативі XML. Альтернативами JSON є XML, YAML, та Protobuf. У Fish Shell для роботи з JSON часто використовують зовнішній інструмент `jq`, який дозволяє здійснювати розгорнутий аналіз та зміну JSON даних.

## Дивіться також
- Офіційний сайт `jq`: https://stedolan.github.io/jq/
- Документація по Fish Shell: https://fishshell.com/docs/current/index.html
- Вивчення JSON: https://www.json.org/json-en.html
