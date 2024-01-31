---
title:                "Робота з YAML"
date:                  2024-01-19
html_title:           "Arduino: Робота з YAML"
simple_title:         "Робота з YAML"

category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Що це & навіщо?
YAML - це формат серіалізації даних, зручний для читання людиною. Програмісти використовують його для конфігурації, документації та обміну даними між сервісами.

## Як це зробити:
В Fish Shell для роботи з YAML зазвичай користуються зовнішніми утилітами, як-от 'yq'. Ось базове використання 'yq' для читання та зміни YAML файлів:

```Fish Shell
# Читаємо значення з YAML
echo 'foo: bar' | yq e '.foo' -

# Змінюємо значення і записуємо в файл
echo 'foo: bar' | yq e '.foo = "baz"' - > output.yaml

# Виводимо результат
cat output.yaml
```

Sample output:
```
foo: baz
```

## Поглиблений огляд:
YAML, що розшифровується як "YAML Ain't Markup Language", з'явився у 2001 році. Зараз це популярна альтернатива JSON і XML через легкість читання і відсутність знаків пунктуації. У Fish Shell для роботи з YAML немає вбудованої підтримки, тому використовують утиліти типу 'yq' (заснована на 'jq' для JSON). 'yq' пропонує розширений синтаксис для маніпулювання YAML файлами.

## Дивись також:
* Офіційний сайт YAML: https://yaml.org
* Репозиторій 'yq': https://github.com/mikefarah/yq
* Документація 'jq': https://stedolan.github.io/jq/manual/
