---
title:                "Склеивание строк"
date:                  2024-01-28T23:55:57.464143-07:00
model:                 gpt-4-0125-preview
simple_title:         "Склеивание строк"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/gleam/concatenating-strings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Конкатенация строк - это соединение двух или более строк встык, чтобы получить новую. Программисты делают это для того, чтобы составлять предложения, смешивать динамические данные с текстом или создавать шаблоны для программной элегантности.

## Как:

Переходим сразу к коду, вот как вы делаете танго со строками в Gleam:

```gleam
fn main() {
  let greeting = "Привет"
  let subject = "Мир"
  let exclamation = "!"

  let message = greeting ++ " " ++ subject ++ exclamation
  message
}

// Ожидаемый вывод: "Привет Мир!"
```

Проще простого, верно? Просто складывайте строки вместе с помощью `++` и у вас получится "суп из строк".

## Погружение в Детали

Конкатенация может показаться простой, но под капотом много всего. Исторически конкатенация строк в языках программирования могла быть сложной из-за различия типов или проблем с неизменяемостью. Альтернативы включают форматирование строк или построение с использованием списков, но конкатенация остается предпочитаемым методом из-за ее простоты.

В Gleam, который высоко ценит чистоту и строгую типизацию, конкатенация строк использует оператор `++`, который гарантирует правильность типов и получение новой строки каждый раз - здесь нет побочных эффектов.

## Смотрите Также

Для большего количества штучек с текстами:

- Введение в Gleam: [https://gleam.run](https://gleam.run)