---
title:                "Gleam: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Чому

Іноді нам потрібно отримати деяку інформацію з інтернету. Наприклад, ми можемо потребувати зберегти в кеші або обробити дані з певної веб-сторінки. Завдання з вдоброю можна виконати, використовуючи Gleam, оскільки він забезпечує зручний і ефективний інструмент для завантаження веб-сторінок.

## Як

Ми можемо завантажувати веб-сторінки за допомогою функції `httpc.get`, яка отримує URL сторінки та повертає його зміст. Наприклад:

```Gleam
let response = httpc.get("https://www.example.com")
```

Також ми можемо вказати додаткові параметри, які визначають метод для запиту, заголовки або тіло запиту. Приклад:

```Gleam
let response =
  httpc.get(
    "https://www.example.com",
    { method: "POST", headers: #{ "Content-Type" => "application/json" }, body: "{ \"name\": \"John\" }" }
  )
```

Результатом буде змінна `response`, яка містить вміст сторінки або повідомлення про помилку у випадку, якщо запрос не був успішним.

## Глибока занурення

У випадку, якщо ми бажаємо отримати більш детальну інформацію про веб-сторінку, наприклад, заголовки або статусну код, ми можемо використати функцію `httpc.request`, яка повертає кортеж з двома елементами: `response` та `request`. Наприклад:

```Gleam
let { response, request } =
  httpc.request(
    "https://www.example.com",
    { method: "GET" }
  )
```

Тепер ми можемо отримати код відповіді, зміст сторінки та інші додаткові дані. Наприклад:

```Gleam
let { code, headers, body } = response
```

Також, ми можемо змінювати стандартні параметри запиту, файл або відправляти запроси асинхронно за допомогою `httpc`, щоб отримати більш розширений функціонал для завантаження веб-сторінок.

# Дивіться також

- [Офіційна документація Gleam про `httpc`](https://gleam.run/api/gleam_stdlib/httpc.html)
- [Приклади коду з використанням `httpc`](https://github.com/gleam-lang/gleam_stdlib/blob/master/repo/examples/httpc_gleam.gr)