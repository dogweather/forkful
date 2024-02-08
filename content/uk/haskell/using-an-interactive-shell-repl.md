---
title:                "Використання інтерактивної оболонки (REPL)"
aliases:
- uk/haskell/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:15:12.061034-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання інтерактивної оболонки (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Що і Чому?
Інтерактивна оболонка або REPL (Read-Eval-Print Loop) у Haskell дозволяє виконувати фрагменти коду наживо. Це майданчик для швидкого зворотного зв'язку, тестування функцій та вивчення мови.

## Як користуватися:
Щоб запустити GHCi (інтерактивне середовище Glasgow Haskell Compiler), просто наберіть `ghci` у вашому терміналі. Ось як це використовувати:

```Haskell
Prelude> let x = 5
Prelude> x * 2
10
Prelude> :t x
x :: Num a => a
```

Приклад виводу пояснює, що `x` - це числова змінна і показує, що її подвоєння дає результат 10.

## Поглиблений огляд:
GHCi в Haskell пройшов довгий шлях з моменту свого створення. Він надає багатий набір можливостей, таких як автодоповнення, введення багаторядкових команд і завантаження пакетів. Альтернативи, такі як Hugs, наразі в більшості є історичними, з GHCi як стандартом. GHCi компілює код у момент введення виразу, надаючи вам ефективний спосіб тестування вашого коду на Haskell.

## Дивіться також:
- [Посібник користувача GHC – GHCi](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html)
- [Вивчай Haskell заради Гарного – Вступ](http://learnyouahaskell.com/starting-out#hello-world)
- [Haskell Wiki – GHC/GHCi](https://wiki.haskell.org/GHC/GHCi)
