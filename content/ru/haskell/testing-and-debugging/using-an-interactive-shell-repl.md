---
title:                "Использование интерактивной оболочки (REPL)"
aliases:
- ru/haskell/using-an-interactive-shell-repl.md
date:                  2024-01-29T00:03:18.941020-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование интерактивной оболочки (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/haskell/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Интерактивная оболочка или REPL (Read-Eval-Print Loop, цикл "чтение-вычисление-вывод") в Haskell позволяет выполнять код в живом режиме. Это площадка для получения быстрой обратной связи, тестирования функций и изучения языка.

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
