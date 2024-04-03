---
date: 2024-01-26 00:52:27.691124-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u041E\u0441\u043D\u043E\u0432\u043D\u0430 \u0444\u0456\u043B\u043E\u0441\u043E\
  \u0444\u0456\u044F Elm \u2013 \u0446\u0435 \"\u0412\u0456\u0434\u0441\u0443\u0442\
  \u043D\u0456\u0441\u0442\u044C \u0432\u0438\u043D\u044F\u0442\u043A\u0456\u0432\
  \ \u043F\u0456\u0434 \u0447\u0430\u0441 \u0432\u0438\u043A\u043E\u043D\u0430\u043D\
  \u043D\u044F\". \u0422\u043E\u043C\u0443 Elm \u0432\u0438\u043A\u043E\u0440\u0438\
  \u0441\u0442\u043E\u0432\u0443\u0454 \u0441\u0432\u043E\u044E \u0442\u0438\u043F\
  \u0456\u0437\u043E\u0432\u0430\u043D\u0443 \u0441\u0438\u0441\u0442\u0435\u043C\u0443\
  \ \u0437 \u0442\u0438\u043F\u0430\u043C\u0438, \u044F\u043A `Maybe` \u0442\u0430\
  \u2026"
lastmod: '2024-03-13T22:44:49.162712-06:00'
model: gpt-4-1106-preview
summary: "\u041E\u0441\u043D\u043E\u0432\u043D\u0430 \u0444\u0456\u043B\u043E\u0441\
  \u043E\u0444\u0456\u044F Elm \u2013 \u0446\u0435 \"\u0412\u0456\u0434\u0441\u0443\
  \u0442\u043D\u0456\u0441\u0442\u044C \u0432\u0438\u043D\u044F\u0442\u043A\u0456\u0432\
  \ \u043F\u0456\u0434 \u0447\u0430\u0441 \u0432\u0438\u043A\u043E\u043D\u0430\u043D\
  \u043D\u044F\"."
title: "\u041E\u0431\u0440\u043E\u0431\u043A\u0430 \u043F\u043E\u043C\u0438\u043B\u043E\
  \u043A"
weight: 16
---

## Як це зробити:
Основна філософія Elm – це "Відсутність винятків під час виконання". Тому Elm використовує свою типізовану систему з типами, як `Maybe` та `Result`, для обробки помилок.

Для сценарію з `Maybe`:

```Elm
safeDivide : Float -> Float -> Maybe Float
safeDivide numerator denominator =
    if denominator == 0 then
        Nothing
    else
        Just (numerator / denominator)
        
-- Коли ви запускаєте:

safeDivide 10 2
--> Just 5

safeDivide 10 0
--> Nothing
```

Для сценарію з `Result`:

```Elm
type Error = DivisionByZero

safeDivide : Float -> Float -> Result Error Float
safeDivide numerator denominator =
    if denominator == 0 then
        Err DivisionByZero
    else
        Ok (numerator / denominator)

-- І використовуючи його:

safeDivide 10 2
--> Ok 5

safeDivide 10 0
--> Err DivisionByZero
```

## Поглиблений огляд
Типова система Elm є суворою, що допомагає виявляти помилки на ранніх етапах. Історично більшість мов спиралися на винятки та перевірки під час виконання, але Elm обрав гарантії під час компіляції. Альтернативи на кшталт `Result` дозволяють деталізувати інформацію про помилки, тоді як `Maybe` простіший для сценаріїв з відповіддю "так-ні". Обробка помилок в Elm заохочує розробників передбачати всі шляхи наперед, уникаючи підводних каменів забутих випадків помилок.

## Дивіться також:
- Розділ офіційного керівництва Elm про обробку помилок: [Обробка помилок - Введення](https://guide.elm-lang.org/error_handling/)
- Документація Elm `Maybe`: [Elm – Maybe](https://package.elm-lang.org/packages/elm/core/latest/Maybe)
- Документація Elm `Result`: [Elm – Result](https://package.elm-lang.org/packages/elm/core/latest/Result)
