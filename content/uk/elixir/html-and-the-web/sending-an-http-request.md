---
date: 2024-01-20 17:59:49.624248-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) \u0412 Elixir, \u043C\u043E\u0436\u043D\u0430 \u0432\u0438\u043A\u043E\u0440\
  \u0438\u0441\u0442\u0430\u0442\u0438 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\
  \u043A\u0443 `HTTPoison` \u0434\u043B\u044F \u0432\u0456\u0434\u043F\u0440\u0430\
  \u0432\u043A\u0438 HTTP \u0437\u0430\u043F\u0438\u0442\u0456\u0432. \u041E\u0441\
  \u044C \u044F\u043A \u0446\u0435 \u043F\u0440\u0430\u0446\u044E\u0454."
lastmod: '2024-04-05T21:53:48.962477-06:00'
model: gpt-4-1106-preview
summary: "(\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438:)\
  \ \u0412 Elixir, \u043C\u043E\u0436\u043D\u0430 \u0432\u0438\u043A\u043E\u0440\u0438\
  \u0441\u0442\u0430\u0442\u0438 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\
  \u0443 `HTTPoison` \u0434\u043B\u044F \u0432\u0456\u0434\u043F\u0440\u0430\u0432\
  \u043A\u0438 HTTP \u0437\u0430\u043F\u0438\u0442\u0456\u0432."
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443"
weight: 44
---

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
