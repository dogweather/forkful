---
title:                "Elixir: Надсилання запиту http з базовою аутентифікацією"
simple_title:         "Надсилання запиту http з базовою аутентифікацією"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Чому

Відправлення HTTP-запиту з основною аутентифікацією може бути важливим для взаємодії з іншими веб-сервісами або для доступу до захищеного вмісту.

## Як

```Elixir
defmodule HTTP do
  @auth %{"username" => "user", "password" => "pass"}
    
  def request(url) do
    response = HTTPoison.get(url, headers: basic_auth(@auth))
    case response do
      {:ok, %{status_code: 200, body: body}} -> IO.puts(body)
      {:ok, %{status_code: 401}} -> IO.puts("Authentication failed.")
      {:error, %HTTPoison.Error{reason: reason}} -> IO.puts(reason)
    end
  end
end
```

Вище наведений код використовує бібліотеку HTTPoison для відправлення GET-запиту на задану URL-адресу з використанням основної аутентифікації. Він перевіряє відповідь сервера, виводить вміст тіла, якщо запит був успішним, або повідомляє про помилку аутентифікації або будь-яку іншу помилку.

## Deep Dive

Основна аутентифікація передбачає включення заголовка "Authorization" з закодованими в Base64 значеннями користувача та пароля. Так як це не є надійним засобом захисту, рекомендується використовувати HTTPS для забезпечення безпеки під час передачі даних.

## Дивіться також

- [HTTPoison документація](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Про основну аутентифікацію](https://developer.mozilla.org/uk/docs/Web/HTTP/Authentication)