---
title:                "Надсилаємо HTTP-запит з базової аутентифікацією"
html_title:           "C#: Надсилаємо HTTP-запит з базової аутентифікацією"
simple_title:         "Надсилаємо HTTP-запит з базової аутентифікацією"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що таке це & Навіщо?
Відправка HTTP-запиту з базовою авторизацією - це процес, коли ваша програма з'єднується з веб-сервером, використовуючи логін та пароль для доступу до ресурсів. Програмісти роблять це, коли вони хочуть отримати дані від веб-сервісів, доступ до яких обмежений.

## Як це робити:
У мові програмування Elixir для цього ми можемо використовувати бібліотеку HTTPoison. Ось приклад використання:

```Elixir
defmodule MyModule do
  def send_request do
    auth = Base.encode64("myusername:mypassword")

    headers = [
      {"Authorization", "Basic #{auth}"}
    ]

    HTTPoison.get!("https://mywebservice.com", headers)
  end
end
```

Цей код відправить HTTP GET запит до `https://mywebservice.com` з іменем користувача та паролем для авторизації.

## Пірнання глибше
Відправка HTTP-запиту з базовою авторизацією - це стандартний спосіб отримання даних від веб-сервісів, який був частиною HTTP стандарту з самого початку. 

Альтернативою цьому є використання OAuth, його варіацій або JWT, які зазвичай використовуються для більш безпечної авторизації. 

Слід зазначити, що базова авторизація передає логін та пароль в незашифрованому вигляді. Використовуйте TLS/SSL для безпечної передачі даних.

## Дивись також
1. Детальніше про HTTPoison ви можете прочитати тут: [HTTPoison README](https://github.com/edgurgel/httpoison)
2. Про OAuth - тут: [OAuth Wikipedia](https://uk.wikipedia.org/wiki/OAuth)
3. Про JWT - тут: [JWT Official Website](https://jwt.io/)