---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:58:44.804146-07:00
description: "\u041A\u0430\u043A: \u041A\u043B\u044E\u0447\u0435\u0432\u0430\u044F\
  \ \u0444\u0438\u043B\u043E\u0441\u043E\u0444\u0438\u044F Elm - \u044D\u0442\u043E\
  \ \u043E\u0442\u0441\u0443\u0442\u0441\u0442\u0432\u0438\u0435 \u0438\u0441\u043A\
  \u043B\u044E\u0447\u0435\u043D\u0438\u0439 \u0432\u043E \u0432\u0440\u0435\u043C\
  \u044F \u0432\u044B\u043F\u043E\u043B\u043D\u0435\u043D\u0438\u044F. \u041F\u043E\
  \u044D\u0442\u043E\u043C\u0443 Elm \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\
  \u0435\u0442 \u0441\u0432\u043E\u044E \u0441\u0438\u0441\u0442\u0435\u043C\u0443\
  \ \u0442\u0438\u043F\u043E\u0432 \u0441 \u0442\u0438\u043F\u0430\u043C\u0438, \u0442\
  \u0430\u043A\u0438\u043C\u0438 \u043A\u0430\u043A `Maybe` \u0438\u2026"
lastmod: '2024-03-13T22:44:44.915423-06:00'
model: gpt-4-0125-preview
summary: "\u041A\u043B\u044E\u0447\u0435\u0432\u0430\u044F \u0444\u0438\u043B\u043E\
  \u0441\u043E\u0444\u0438\u044F Elm - \u044D\u0442\u043E \u043E\u0442\u0441\u0443\
  \u0442\u0441\u0442\u0432\u0438\u0435 \u0438\u0441\u043A\u043B\u044E\u0447\u0435\u043D\
  \u0438\u0439 \u0432\u043E \u0432\u0440\u0435\u043C\u044F \u0432\u044B\u043F\u043E\
  \u043B\u043D\u0435\u043D\u0438\u044F."
title: "\u041E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0430 \u043E\u0448\u0438\u0431\
  \u043E\u043A"
weight: 16
---

## Как:
Ключевая философия Elm - это отсутствие исключений во время выполнения. Поэтому Elm использует свою систему типов с типами, такими как `Maybe` и `Result`, для обработки ошибок.

Для сценария `Maybe`:

```Elm
safeDivide : Float -> Float -> Maybe Float
safeDivide числитель знаменатель =
    if знаменатель == 0 then
        Nothing
    else
        Just (числитель / знаменатель)
        
-- При запуске:

safeDivide 10 2
--> Just 5

safeDivide 10 0
--> Nothing
```

Для сценария `Result`:

```Elm
type Error = DivisionByZero

safeDivide : Float -> Float -> Result Error Float
safeDivide числитель знаменатель =
    if знаменатель == 0 then
        Err DivisionByZero
    else
        Ok (числитель / знаменатель)

-- Используя это:

safeDivide 10 2
--> Ok 5

safeDivide 10 0
--> Err DivisionByZero
```

## Погружение
Система типов Elm строга, что помогает обнаруживать ошибки на ранней стадии. Исторически большинство языков полагались на исключения и проверки во время выполнения, но Elm выбрал гарантии на этапе компиляции. Альтернативы вроде `Result` позволяют передавать подробную информацию об ошибках, тогда как `Maybe` проще для сценариев да-нет. Обработка ошибок в Elm побуждает разработчиков заранее учитывать все возможные пути, избегая ловушек забытых случаев ошибок.

## Смотрите также:
- Официальный раздел руководства Elm по обработке ошибок: [Обработка ошибок – Введение](https://guide.elm-lang.org/error_handling/)
- Документация Elm `Maybe`: [Elm – Maybe](https://package.elm-lang.org/packages/elm/core/latest/Maybe)
- Документация Elm `Result`: [Elm – Result](https://package.elm-lang.org/packages/elm/core/latest/Result)
