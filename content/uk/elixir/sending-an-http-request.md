---
title:                "Elixir: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Чому

У сучасному світі, веб-розробка є необхідною умінням для багатьох програмістів. Високорівневі мови програмування, такі як Elixir, надають зручні інструменти для здійснення HTTP-запитів. Це дозволяє зв'язувати наші додатки з іншими системами та сервісами, що значно розширює можливості наших програм. У цій статті ми розглянемо, як надсилати HTTP-запити за допомогою Elixir та як це може бути корисно.

## Як

Для надсилання HTTP-запитів у Elixir ми будемо використовувати бібліотеку `HTTPoison`. Перш ніж почати, переконайтеся, що у вас встановлений Elixir та отримайте бібліотеку за допомогою залежностей Mix:

```Elixir
def deps do
  [
    {:httpoison, "~> 1.6"}
  ]
end
```

Далі, імпортуємо `HTTPoison` та встановимо налаштування, яке дозволить нам отримувати дані в форматі JSON:

```Elixir
iex> import HTTPoison

iex> httpoison.set(json_decoder: Poison)
```

Тепер, для надсилання GET-запита, ми можемо використати функцію `get` із необхідними параметрами, такими як URL та заголовки:

```Elixir
iex> result = get("https://jsonplaceholder.typicode.com/posts", [{"Content-Type", "application/json"}])

iex> result
%HTTPoison.Response{status_code: 200, body: "[{...}, {...}, ...]"}
```

Як бачимо, ми отримали відповідь у форматі JSON, яку ми можемо подальше обробити за допомогою функцій `Poison`.

## Deep Dive

Для надсилання інших типів запитів, таких як POST, PUT чи DELETE, ми можемо використовувати відповідні функції `post`, `put` та `delete`, у яких також можна передавати параметри та тіло запиту.

За допомогою опції `stream_to` ми можемо також надсилати файл для POST-запитів:

```Elixir
iex> file = File.read("test.json")

iex> post("https://jsonplaceholder.typicode.com/posts", file, [{"Content-Type", "application/json"}], stream_to: self())

iex> receive do
...>   {:httpoison_response, %{status_code: 201, body: body}} -> body
...> end
"{...}"
```

Більш детальну інформацію про параметри та опції можна знайти у документації `HTTPoison`.

## Дивись також

- [Документація HTTPoison](https://hexdocs.pm/httpoison)
- [Туторіал з HTTP-запитів у Elixir](https://elixirschool.com/lessons/advanced/http/)
- [Бібліотека HTTPoison на GitHub](https://github.com/edgurgel/httpoison)