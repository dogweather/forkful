---
title:                "Відправлення http-запиту"
html_title:           "Gleam: Відправлення http-запиту"
simple_title:         "Відправлення http-запиту"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що & Чому?

В програмуванні часто потрібно здійснювати HTTP запити. Це є процесом відправки запиту до веб-сервера та отримання відповіді. Програмісти це роблять, щоб отримати доступ до даних з інших джерел або взаємодіяти з різними сервісами.

## Як зробити:

Щоб зробити HTTP запит у Gleam, можна використовувати вбудований модуль `gleam/http`. Давайте подивимося на приклад коду, який відправить запит до GitHub API та отримає список усіх публічних репозиторіїв користувача `gleam-lang`:

```
Gleam.http.send(
  "https://api.github.com/users/gleam-lang/repos",
  "GET"
)
|> Gleam.http.to_result
|> case {
  Ok(response) -> response.body
  Err(error) -> error_to_string(error)
}
```

На виході ми отримаємо список у форматі JSON, який можна подальше обробити за потреби.

## Глибші дослідження:

HTTP - це протокол, який був розроблений у 1990-х роках і з тих пір став стандартом для комунікації між веб-серверами та клієнтами. У програмуванні є інші інструменти для взаємодії зі зовнішнім світом, такі як WebSockets або UDP протокол, але HTTP є одним з найпоширеніших.

Як альтернативи, програмісти можуть використовувати бібліотеки на мовах програмування, таких як Python або Java, для здійснення HTTP запитів. Однак, у Gleam це можна зробити безпосередньо за допомогою вбудованого модуля `gleam/http`, що дозволяє досягти більш ефективного та швидкого виконання запитів.

## Дивіться також:

- Офіційна документація Gleam: https://gleam.run/documentation
- Репозиторій Gleam у GitHub: https://github.com/gleam-lang/gleam
- Розділ про HTTP запити у документації Erlang: http://erlang.org/doc/apps/inets/http_client.html