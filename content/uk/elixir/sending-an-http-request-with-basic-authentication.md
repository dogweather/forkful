---
title:                "Надсилання HTTP-запиту з базовою автентифікацією"
date:                  2024-01-20T18:02:02.848395-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту з базовою автентифікацією"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що це & Чому?

Надсилання HTTP-запиту з базовою автентифікацією — це процес, де клієнт додає до своєї вимоги логін і пароль для доступу до захищених ресурсів. Програмісти роблять це, щоб забезпечити безпечний обмін даними.

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