---
title:                "Завантаження веб-сторінки"
html_title:           "Gleam: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Чому 

Веб-сторінки є головними інструментами для отримання інформації в сучасному світі. Завантаження веб-сторінок є ключовим етапом у використанні та аналізі цієї інформації.

## Як 

Gleam має можливість завантажувати веб-сторінки з використанням вбудованого модуля "httpc". Для цього вам потрібно встановити пакет "gleam/httpc 1.0.0" та імпортувати його до свого модуля.

```Gleam
import httpc as http

http.get("https://www.example.com/")
|> http.send()
|> case _ {
  Ok(resp) -> 
    resp.body
    |> String.from_list()
    |> assert.equal("Hello World!")

  Error(_err) -> assert.fail("Failed to fetch page")
}
```

В цьому прикладі ми використовуємо функцію "get", щоб загрузити веб-сторінку із URL-адреси. Потім ми використовуємо результат для перевірки вмісту сторінки за допомогою функції "equal".

Можна додатково налаштувати запит, передаючи параметри в функцію "get". Наприклад, ви можете використовувати параметр "headers" для встановлення заголовків запиту або параметр "query_params" для додавання параметрів до URL-адреси.

## Глибокий занурення 

Загрузка веб-сторінки - це більше, ніж просто завантаження вмісту. Це також може бути корисною у випадках, коли вам потрібно отримати дані з API або взаємодіяти з веб-додатками.

Наприклад, ви можете використати функції "post" або "patch" для відправки POST або PATCH запитів до сервера. Також, ви можете використовувати функцію "send_with_incomplete" для відправки незавершеного запиту, що дозволяє потокову обробку великих обсягів даних.

Щоб отримати більше інформації про функції та можливості "httpc", ви можете звернутися до офіційної документації Gleam.

## Дивіться також 

- [Офіційна документація Gleam] (https://gleam.run/basic_types)
- [Пакет "gleam/httpc"] (https://github.com/gleam-lang/httpc)