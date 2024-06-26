---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:03:18.941020-07:00
description: "\u041A\u0430\u043A \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\
  \u0430\u0442\u044C: \u0427\u0442\u043E\u0431\u044B \u0437\u0430\u043F\u0443\u0441\
  \u0442\u0438\u0442\u044C GHCi (\u0438\u043D\u0442\u0435\u0440\u0430\u043A\u0442\u0438\
  \u0432\u043D\u043E\u0435 \u043E\u043A\u0440\u0443\u0436\u0435\u043D\u0438\u0435\
  \ \u043A\u043E\u043C\u043F\u0438\u043B\u044F\u0442\u043E\u0440\u0430 Glasgow Haskell),\
  \ \u043F\u0440\u043E\u0441\u0442\u043E \u0432\u0432\u0435\u0434\u0438\u0442\u0435\
  \ `ghci` \u0432 \u0442\u0435\u0440\u043C\u0438\u043D\u0430\u043B\u0435. \u0412\u043E\
  \u0442 \u043A\u0430\u043A \u044D\u0442\u043E \u0438\u0441\u043F\u043E\u043B\u044C\
  \u0437\u043E\u0432\u0430\u0442\u044C."
lastmod: '2024-03-13T22:44:45.136796-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u043E\u0431\u044B \u0437\u0430\u043F\u0443\u0441\u0442\u0438\
  \u0442\u044C GHCi (\u0438\u043D\u0442\u0435\u0440\u0430\u043A\u0442\u0438\u0432\u043D\
  \u043E\u0435 \u043E\u043A\u0440\u0443\u0436\u0435\u043D\u0438\u0435 \u043A\u043E\
  \u043C\u043F\u0438\u043B\u044F\u0442\u043E\u0440\u0430 Glasgow Haskell), \u043F\u0440\
  \u043E\u0441\u0442\u043E \u0432\u0432\u0435\u0434\u0438\u0442\u0435 `ghci` \u0432\
  \ \u0442\u0435\u0440\u043C\u0438\u043D\u0430\u043B\u0435."
title: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\
  \ \u0438\u043D\u0442\u0435\u0440\u0430\u043A\u0442\u0438\u0432\u043D\u043E\u0439\
  \ \u043E\u0431\u043E\u043B\u043E\u0447\u043A\u0438 (REPL)"
weight: 34
---

## Как использовать:
Чтобы запустить GHCi (интерактивное окружение компилятора Glasgow Haskell), просто введите `ghci` в терминале. Вот как это использовать:

```Haskell
Prelude> let x = 5
Prelude> x * 2
10
Prelude> :t x
x :: Num a => a
```

Пример вывода объясняет, что `x` является числовой переменной и показывает, что удвоение его результата приводит к 10.

## Глубокое погружение:
GHCi Haskell прошел долгий путь с момента своего создания. Он предоставляет богатый набор функций, таких как автодополнение, многострочный ввод и загрузка пакетов. Альтернативы, такие как Hugs, на данный момент в основном исторические, с GHCi являющимися стандартом. GHCi компилирует код в режиме реального времени каждый раз, когда вы вводите выражение, предоставляя вам эффективный способ тестирования вашего кода на Haskell.

## Смотрите также:
- [Руководство пользователя GHC – GHCi](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html)
- [Учим Haskell ради большого хорошего! – Начало работы](http://learnyouahaskell.com/starting-out#hello-world)
- [Вики Haskell – GHC/GHCi](https://wiki.haskell.org/GHC/GHCi)
