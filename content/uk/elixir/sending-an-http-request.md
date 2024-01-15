---
title:                "Надсилання http запиту"
html_title:           "Elixir: Надсилання http запиту"
simple_title:         "Надсилання http запиту"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Чому

Надсилання HTTP запиту - це важлива частина розробки веб-додатків, так як ця протокол дозволяє програмам обмінюватися даними через мережу Інтернет. Зокрема, це дозволяє отримати від сервера HTML сторінку, яку ми бачимо в браузері, або отримати дані з сервера для подальшої обробки.

## Як

Надсилання HTTP запиту в Еліксир можливо завдяки використанню модуля `HTTPoison`. Перш за все, необхідно встановити цей модуль за допомогою команди `mix deps.get`. Далі, можна використовувати функцію `HTTPoison.request/4`, передаючи у неї параметри `:post` або `:get` для надсилання відповідних запитів, а також URL адресу та необов'язково дані для надсилання. Нижче подані приклади коду та можливі виходи для відповідних запитів.

```Elixir
# Відправка GET запиту
HTTPoison.request(:get, "https://jsonplaceholder.typicode.com/posts/1")

# Вихід:
{:ok, %HTTPoison.Response{body: "{\"userId\": 1, \"id\": 1, \"title\": \"sunt aut facere repellat provident occaecati excepturi optio reprehenderit\", \"body\": \"quia et suscipit\\nsuscipit recusandae consequuntur expedita et cum\\nreprehenderit molestiae ut ut quas totam\\nnostrum rerum est autem sunt rem eveniet architecto\"}", headers: [...], request: %HTTPoison.Request{...}, status_code: 200}}

# Відправка POST запиту
HTTPoison.request(:post, "https://jsonplaceholder.typicode.com/posts", [], body: %{title: "New post", body: "This is a new post!", userId: 1})

# Вихід:
{:ok, %HTTPoison.Response{body: "{\"title\": \"New post\", \"body\": \"This is a new post!\", \"userId\": 1, \"id\": 101}", headers: [...], request: %HTTPoison.Request{...}, status_code: 201}}
```

## Глибинне дослідження

На практиці можуть виникнути ситуації, коли потрібно вказати додаткові параметри для запиту, наприклад, заголовки або авторизацію. Для цього, можна передати додатковий аргумент `headers` або `basic_auth` у функцію `HTTPoison.request/4`. Також, можна використовувати інші методи HTTP запитів (наприклад, `:put` чи `:delete`) та передавати дані у JSON форматі за допомогою бібліотеки `Poison`.

## Дивіться також

- [Документація по HTTPoison](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Офіційний сайт Еліксир](https://elixir-lang.org/)
- [Приклади коду для надсилання HTTP запитів в Еліксирі](https://gist.github.com/elliottneilclark/5d0b116e379e6e862872)