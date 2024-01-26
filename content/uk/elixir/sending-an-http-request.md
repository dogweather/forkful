---
title:                "Надсилання HTTP-запиту"
date:                  2024-01-20T17:59:49.624248-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)

HTTP запит – це прохання до сервера за ресурсами, наприклад, веб-сторінкою або API-ендпоінтом. Програмісти використовують HTTP запити для взаємодії зі вдаленими системами та обміну даними.

## How to: (Як це зробити:)

В Elixir, можна використати бібліотеку `HTTPoison` для відправки HTTP запитів. Ось як це працює:

```elixir
# Додайте HTTPoison у ваш mix.exs файл
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end

# Час запустити інтерактивну Elixir оболонку (iex) та відправити запит.

iex> HTTPoison.start()
:ok

iex> response = HTTPoison.get!("https://jsonplaceholder.typicode.com/posts/1")
%HTTPoison.Response{
  body: "{
    \"userId\": 1,
    \"id\": 1,
    \"title\": \"sunt aut facere repellat provident occaecati excepturi optio reprehenderit\",
    \"body\": \"quia et suscipit...\"}",
  status_code: 200
}

iex> response.body
"{
  \"userId\": 1,
  \"id\": 1,
  \"title\": \"sunt aut facere repellat provident occaecati excepturi optio reprehenderit\",
  \"body\": \"quia et suscipit...\"
}"
```

## Deep Dive (Поглиблене занурення):

HTTP запити з'явилися з початком World Wide Web і використовуються для взаємодії із серверами у всьому Інтернеті. В Elixir, можна також використати інші бібліотеки, як `Tesla` або нижньорівневі модулі, як `:httpc`, що є частиною Erlang стандартної бібліотеки. Деталі виконання HTTP запитів заглиблюють у TCP/IP протоколи, зокрема, як HTTP працює поверх них. Сучасні HTTP-клієнти Elixir дозволяють легко налаштовувати запити, додавати таймаути, обробляти відповіді тощо.

## See Also (Дивіться також):

- Офіційна документація для `HTTPoison`: https://hexdocs.pm/httpoison/
- GitHub сторінка `Tesla`, альтернативної бібліотеки HTTP клієнта: https://github.com/teamon/tesla
- Довідник `:httpc` модуля Erlang: http://erlang.org/doc/man/httpc.html
