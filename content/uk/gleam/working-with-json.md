---
title:                "Робота з json"
html_title:           "Gleam: Робота з json"
simple_title:         "Робота з json"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/working-with-json.md"
---

{{< edit_this_page >}}

# Що & Чому?

JSON - це спосіб представлення даних у вигляді тексту. Використання JSON є популярним у програмуванні, оскільки дозволяє зберігати та передавати дані між різними програмами та платформами.

Програмісти використовують JSON, щоб зручно взаємодіяти зі зовнішніми джерелами даних, такими як веб-сервіси чи бази даних. Також, це дає можливість легко обробляти та аналізувати дані у своїх програмах.

# Як?

Використовуючи мову програмування Gleam, ми можемо легко читати та створювати дані у форматі JSON. Для цього нам потрібно використовувати модуль Json у нашому коді.

```Gleam
let my_data = Json.Encode.object([
  ("name", Json.Encode.string("John")),
  ("age", Json.Encode.int(25))
])

my_data
```

Результат:

```
{name: "John", age: 25}
```

Ми також можемо прочитати дані з JSON у нашому коді:

```Gleam
let my_data = Json.Decode.object(data)

my_data["name"]
```

Результат:

```
"John"
```

# Поглиблене дослідження

JSON був створений у 2001 році та швидко став популярним у світі програмування. Його використання поширилося на багато мов програмування та платформ.

У Gleam є альтернативи для роботи з JSON, такі як бібліотека iJSON, яка надає більше можливостей для обробки та перетворення даних у форматі JSON.

Реалізація модуля Json у Gleam базується на стандарті RFC 7159, що гарантує сумісність з більшістю інших мов та платформ.

# Дивись також

- Документація з Json у Gleam: https://gleam.run/modules/json/latest/
- Стандарт RFC 7159: https://tools.ietf.org/html/rfc7159
- Документація з бібліотекою iJSON: https://github.com/michalmuskala/ijson