---
title:                "Робота з JSON"
date:                  2024-01-19
html_title:           "Arduino: Робота з JSON"
simple_title:         "Робота з JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/working-with-json.md"
---

{{< edit_this_page >}}

## Що і Навіщо?
JSON - це формат обміну даними, який легко читається людьми і машинами. Програмісти використовують його для легкого обміну даними між серверами і web-додатками.

## Як це робиться:
Спершу встановимо `jq` - потужний обробник JSON для Bash:

```Bash
sudo apt-get install jq
```

Читаємо JSON із файлу і отримуємо значення за ключем:

```Bash
echo '{"name": "Олег", "age": 30}' | jq '.name'
```

Вивід:

```Bash
"Олег"
```

Створюємо новий JSON об'єкт:

```Bash
echo '{}' | jq '. + {"city": "Київ"}'
```

Вивід:

```Bash
{
  "city": "Київ"
}
```

## Поглиблено:
`jq` з'явився у 2012 році і став альтернативою для роботи з JSON в командній строці. Інші інструменти включають `jshon` та `json.tool` (Python модуль). `jq` підтримує складні запити, такі як фільтрація та зведення даних, а також має настроювану глибину обробки.

## Додатково:
- Офіційний сайт `jq`: [stedolan.github.io/jq](https://stedolan.github.io/jq/)
- Документація `jq`: [stedolan.github.io/jq/manual/](https://stedolan.github.io/jq/manual/)
