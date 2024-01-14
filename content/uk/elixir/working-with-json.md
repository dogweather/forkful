---
title:                "Elixir: Робота з json"
simple_title:         "Робота з json"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Чому

У цій статті ми дізнаємося, чому робота з JSON є важливою навичкою для мови програмування Elixir. JSON є одним з найпопулярніших форматів обміну даними в сучасному веб-розробці, тому знання його обробки є корисним для будь-якого програміста.

## Якість

Для роботи з JSON у Elixir нам потрібно скористатися модулем `Jason`. Спочатку нам потрібно встановити його, виконавши команду `mix deps.get`. Потім ми можемо починати працювати з JSON.

```elixir
# конвертація Elixir структури у JSON
Jason.encode(%{name: "John", age: 27})
# вивід: "{\"name\":\"John\",\"age\":27}"

# конвертація JSON у Elixir структуру
Jason.decode("{\"name\":\"John\",\"age\":27}")
# вивід: %{name: "John", age: 27}
```

## Глибоке занурення

Окрім простої обробки JSON, Elixir також має потужні інструменти для глибшого вивчення даних. Модуль `Poison` дозволяє працювати з JSON в динамічному режимі, що дозволяє створювати та редагувати структури даних в Elixir без необхідності використовувати серіалізацію.

```elixir
# створення нової пустої динамічної структури
data = Poison.Parser.parse("{}")
# вивід: %Poison.Parser.Dynamic{data: %{}, wraps: nil}

# додавання нового ключа до структури
Poison.Dynamic.add_key(data, :age, 27)
# вивід: %Poison.Parser.Dynamic{data: %{age: 27}, wraps: nil}

# конвертація в JSON
Poison.encode!(data)
# вивід: "{\"age\":27}"
```

## Дивись також

Для детальнішої інформації про роботу з JSON у Elixir, можна переглянути офіційну документацію на сайті [Elixir School](https://elixirschool.com/lessons/specifics/json/).

Також, варто звернути увагу на інші корисні бібліотеки для роботи з JSON, такі як `Poison` та `Jazz`. Ресурси з вікторкого коду та прикладів можна знайти на сайтах [Elixir Bookshelf](https://elixirbookshelf.com/json-parsing-in-elixir) та [Elixir Cookbook](https://elixircookbook.com/blog/working-with-json-in-elixir).

Happy coding! Слава Еликсиру!