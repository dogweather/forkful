---
date: 2024-01-20 18:02:02.848395-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0411\u0430\u0437\u043E\u0432\u0430 \u0430\u0432\u0442\u0435\u043D\u0442\u0438\
  \u0444\u0456\u043A\u0430\u0446\u0456\u044F \u2014 \u0446\u0435 \u0441\u0442\u0430\
  \u0440\u0430, \u0430\u043B\u0435 \u043F\u0440\u043E\u0441\u0442\u0430 \u0441\u0445\
  \u0435\u043C\u0430 \u0430\u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\
  \u0446\u0456\u0457. \u0412\u043E\u043D\u0430 \u043F\u0435\u0440\u0435\u0434\u0431\
  \u0430\u0447\u0430\u0454 \u0432\u0456\u0434\u043F\u0440\u0430\u0432\u043B\u0435\u043D\
  \u043D\u044F \u0456\u043C\u0435\u043D\u0456 \u043A\u043E\u0440\u0438\u0441\u0442\
  \u0443\u0432\u0430\u0447\u0430 \u0442\u0430 \u043F\u0430\u0440\u043E\u043B\u044E\
  \ \u0437 \u043A\u043E\u0436\u043D\u0438\u043C\u2026"
lastmod: '2024-04-05T21:53:48.966481-06:00'
model: gpt-4-1106-preview
summary: "\u0411\u0430\u0437\u043E\u0432\u0430 \u0430\u0432\u0442\u0435\u043D\u0442\
  \u0438\u0444\u0456\u043A\u0430\u0446\u0456\u044F \u2014 \u0446\u0435 \u0441\u0442\
  \u0430\u0440\u0430, \u0430\u043B\u0435 \u043F\u0440\u043E\u0441\u0442\u0430 \u0441\
  \u0445\u0435\u043C\u0430 \u0430\u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\
  \u0430\u0446\u0456\u0457."
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E"
weight: 45
---

## Як це зробити:
```elixir
defmodule HTTPClient do
  require Logger

  @url "http://example.com"
  @username "user"
  @password "pass"

  def send_request do
    :httpc.request(
      :get,
      {to_charlist(@url), []},
      [autoredirect: true, 
        headers: ["Authorization": basic_auth_header(@username, @password)]
      ],
      []
    )
  end

  defp basic_auth_header(username, password) do
    "Basic " <> Base.encode64("#{username}:#{password}")
  end
end

# Sample output
{:ok,
 {{'HTTP/1.1', 200, 'OK'},
  [{"content-type", "application/json; charset=utf-8"}],
  '...response body...'}}
```

## Поглиблений огляд
Базова автентифікація — це стара, але проста схема автентифікації. Вона передбачає відправлення імені користувача та паролю з кожним запитом. Еліксир використовує модуль `:httpc`, який входить до Erlang/OTP, і підтримує базову автентифікацію. При базовій автентифікації потрібно кодувати логін і пароль у форматі Base64.

Є альтернативи безпечніші, як OAuth 2.0, але для простих чи внутрішніх додатків базова автентифікація іноді все ще використовується. Важливо не забувати, що базова автентифікація без HTTPS небезпечна, оскільки логін і пароль можуть бути легко перехоплені.

## Дивись також:
- Elixir's `HTTPoison` library documentation: [https://hexdocs.pm/httpoison](https://hexdocs.pm/httpoison)
- Official `:httpc` module documentation: [http://erlang.org/doc/man/httpc.html](http://erlang.org/doc/man/httpc.html)
- HTTP Basic Authentication on Mozilla Developer Network: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
- Guide to HTTP authentication with Elixir's `Plug`: [https://hexdocs.pm/plug/Plug.Conn.html#module-basic-authentication](https://hexdocs.pm/plug/Plug.Conn.html#module-basic-authentication)
