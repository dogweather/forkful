---
title:                "Elixir: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Чому

Завантаження веб-сторінки може бути необхідним для отримання інформації або виконання певних дій на інтернет-ресурсах. Наприклад, ви можете хотіти оновити дані на своєму веб-сайті або зберегти контент для подальшого використання.

## Як це зробити

Щоб завантажити веб-сторінку за допомогою мови програмування Elixir, використовуйте наступний код:

```
Elixir
url = "https://www.example.com"
{:ok, body} = HTTPoison.get(url)
IO.inspect body

```

Приклад вихідних даних:

```
Elixir
"<html><body><h1>Hello, world!</h1></body></html>"
```

## Глибоке дослідження

Завантаження веб-сторінки може бути більш складною задачею, ніж просто отримання її вмісту. Можна використовувати додаткові бібліотеки, такі як Hound або Hackney, для роботи з формами, кукісами та іншими елементами веб-сторінки. Також важливо розуміти стандартні протоколи, такі як HTTP і HTTPS, та правильно обробляти різні відповіді сервера.

## Дивись також

- [HTTPoison офіційна документація](https://hexdocs.pm/httpoison/readme.html)
- [Hound офіційна документація](https://hexdocs.pm/hound/readme.html)
- [Hackney офіційна документація](https://hexdocs.pm/hackney/readme.html)
- [ElixirForum - спільнота програмістів Elixir](https://elixirforum.com)