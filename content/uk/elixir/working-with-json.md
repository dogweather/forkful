---
title:                "Робота з json"
html_title:           "Elixir: Робота з json"
simple_title:         "Робота з json"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Чому

Еліксир є потужним мовою програмування, яка пропонує зручні інструменти для роботи з JSON-структурами даних. Це дозволяє програмістам легко обробляти інформацію, що зберігається у форматі JSON, з метою подальшої обробки та використання.

## Як

Кодування та розкодування JSON даних в Еліксир може бути легко здійснене за допомогою вбудованих функцій. Наприклад, для кодування об'єктів JSON до рядків, використовуйте функцію `Jason.encode!/1`:

```
Elixir
users = %{"name" => "John", "age" => 30}
Jason.encode!(users)
```

В результаті отримаємо наступний вихід:

```
"{\"name\":\"John\",\"age\":30}"
```

Для розкодування рядка у JSON форматі у структуру даних Еліксир, можна використати функцію `Jason.decode!/1`:

```
Elixir
json = "{\"name\":\"John\",\"age\":30}"
Jason.decode!(json)
```

Отримаємо наступний вихід:

```
%{"name" => "John", "age" => 30}
```

## Глибина дослідження

Крім вбудованих функцій, Еліксир також пропонує різноманітні бібліотеки для роботи з JSON. Наприклад, `Poison` та `Jiffy` є широко використовуваними для швидкого та ефективного парсингу та кодування JSON даних.

Можна розглянути й інші бібліотеки та їх можливості для вибору найкращого варіанту для вашого проекту.

## Дивіться також

- [Документація Еліксир щодо роботи з JSON](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html)
- [Бібліотека Poison](https://hexdocs.pm/poison/readme.html)
- [Бібліотека Jiffy](https://hexdocs.pm/jiffy/readme.html)