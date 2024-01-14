---
title:                "Gleam: Відправлення http запиту."
simple_title:         "Відправлення http запиту."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## З чого почати

Наш веб-додаток може бути зв'язаний із зовнішнім ресурсом, наприклад, для отримання даних про погоду або парсингу інформації. Для цього буде потрібно надіслати HTTP-запит. Сьогодні ми поговоримо про те, як це робити у мові програмування Gleam.

## Як це зробити

Перш ніж розпочати, нам потрібно додати пакет `httpc` до нашого проекту Gleam. Для цього запустіть наступну команду:

```
gleam install gleam/httpc
```

Тепер, коли ми маємо правильний пакет, ми можемо створити HTTP-запит за допомогою наступного коду:

```gleam
import httpc
import gleam/httpc.{Request, Method}

let request =
  Request.new(Method.Get, "https://example.com")

let response ~ok, ~error =
  case {ok, error} of
    {Some(reply), _} -> io.println(reply.body)
    {_, Some(err)} -> io.format("%p\n", [err])

httpc.request(request, response)
```

Цей код створить запит GET до https://example.com і, якщо все пройде успішно, виведе тіло відповіді у консоль. Якщо ж виникне помилка, вона буде виведена також.

## Глибше занурення

У цьому прикладі, ми використовуємо метод `request` з модуля `httpc`, щоб надіслати наш запит і вказати функцію обратного виклику, яка буде виконана після отримання відповіді. Ми також можемо встановити додаткові налаштування запросу, наприклад, заголовки або використовувати інші методи, такі як POST або PUT.

Важливо знати, що модуль `httpc` має бути імпортований та використаний у ваших проектах до виконання функції `httpc.request`.

## Дивіться також

- [Документація Gleam щодо HTTP-запитів](https://gleam.run/modules/httpc.html)
- [Приклади коду Gleam](https://github.com/gleam-lang/gleam/tree/master/examples)