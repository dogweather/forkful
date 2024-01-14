---
title:                "C++: Відправлення http-запиту"
simple_title:         "Відправлення http-запиту"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Чому

Пересилка HTTP-запиту є важливою частиною програмування веб-додатків, оскільки вона дозволяє отримувати та передавати дані з сервера.

## Як

Для початку потрібно підключити необхідну бібліотеку, наприклад, "iostream". Далі необхідно створити змінну типу "http::request" та вказати метод, шлях і тип даних, які необхідно передати. Наприклад:

```C++
#include <iostream>
#include <cpp-httplib> // бібліотека для пересилки HTTP-запитів

http::request req("GET", "/users", "application/json");

// виконання запиту і збереження результату
auto res = req.send("example.com");
```

Виконуючи цей код, наш додаток відправить GET-запит за вказаним шляхом до вказаного сервера та отримає результат у форматі JSON.

## Deep Dive

При відправці HTTP-запиту, важливо вказати кодування даних, яке використовується для передачі інформації між сервером та клієнтом. Для цього можна використовувати функцію "set_header", вказавши необхідне кодування, наприклад:

```C++
auto res = req.set_header("Content-Type", "application/json").send("example.com");
```

Крім того, можна використовувати такі методи, як PUT або POST, для відправки та збереження даних на сервері.

## Зобраити також

- [Cpp-httplib documentation](https://cpp-httplib.readthedocs.io/en/latest/index.html)
- [HTTP Request in C++: How to Send GET/POST Requests](https://www.freecodecamp.org/news/how-to-send-an-http-request-in-cpp/)
- [Introduction to HTTP Requests with C++](https://www.codeproject.com/Articles/33882/Introduction-to-HTTP-Requests-with-C)