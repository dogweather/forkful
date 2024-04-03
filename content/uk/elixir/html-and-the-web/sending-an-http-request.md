---
date: 2024-01-20 17:59:49.624248-07:00
description: "HTTP \u0437\u0430\u043F\u0438\u0442 \u2013 \u0446\u0435 \u043F\u0440\
  \u043E\u0445\u0430\u043D\u043D\u044F \u0434\u043E \u0441\u0435\u0440\u0432\u0435\
  \u0440\u0430 \u0437\u0430 \u0440\u0435\u0441\u0443\u0440\u0441\u0430\u043C\u0438\
  , \u043D\u0430\u043F\u0440\u0438\u043A\u043B\u0430\u0434, \u0432\u0435\u0431-\u0441\
  \u0442\u043E\u0440\u0456\u043D\u043A\u043E\u044E \u0430\u0431\u043E API-\u0435\u043D\
  \u0434\u043F\u043E\u0456\u043D\u0442\u043E\u043C. \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u0456\u0441\u0442\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\
  \u043E\u0432\u0443\u044E\u0442\u044C HTTP \u0437\u0430\u043F\u0438\u0442\u0438 \u0434\
  \u043B\u044F \u0432\u0437\u0430\u0454\u043C\u043E\u0434\u0456\u0457 \u0437\u0456\
  \u2026"
lastmod: '2024-03-13T22:44:48.718305-06:00'
model: gpt-4-1106-preview
summary: "HTTP \u0437\u0430\u043F\u0438\u0442 \u2013 \u0446\u0435 \u043F\u0440\u043E\
  \u0445\u0430\u043D\u043D\u044F \u0434\u043E \u0441\u0435\u0440\u0432\u0435\u0440\
  \u0430 \u0437\u0430 \u0440\u0435\u0441\u0443\u0440\u0441\u0430\u043C\u0438, \u043D\
  \u0430\u043F\u0440\u0438\u043A\u043B\u0430\u0434, \u0432\u0435\u0431-\u0441\u0442\
  \u043E\u0440\u0456\u043D\u043A\u043E\u044E \u0430\u0431\u043E API-\u0435\u043D\u0434\
  \u043F\u043E\u0456\u043D\u0442\u043E\u043C."
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443"
weight: 44
---

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
