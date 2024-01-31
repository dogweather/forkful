---
title:                "Интерполяция строки"
date:                  2024-01-28T23:58:56.239575-07:00
model:                 gpt-4-0125-preview
simple_title:         "Интерполяция строки"

category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/haskell/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Интерполяция строк позволяет внедрять переменные напрямую в строки. Это делается для удобства и читаемости — не нужны ни плюсы, ни вызовы функций, чтобы сконструировать ваше сообщение.

## Как это сделать:

В Haskell интерполяция строк не встроена изначально, но с пакетом `interpolate` вы можете приблизиться к желаемому. Сначала убедитесь, что у вас установлен пакет:

```bash
cabal update
cabal install interpolate
```

Теперь напишите немного кода на Haskell:

```haskell
{-# LANGUAGE QuasiQuotes #-}
import Data.String.Interpolate (i)

main :: IO ()
main = do
    let name = "world"
    let greeting = [i|Привет, #{name}!|]
    putStrLn greeting
```

Запустите его:

```
Привет, мир!
```

## Погружение в тему

Исторически Haskell не включал интерполяцию строк из коробки. Это функциональность более распространена в скриптовых языках. Интерполяция в Haskell стала более гладкой с развитием квазикотировщиков, которые позволяют определить собственный пользовательский синтаксис — как наш `i` для интерполяции строк.

Альтернативы? Конечно, используйте `printf` из `Text.Printf`, или конкатенируйте строки и переменные с помощью `++`. Но эти методы не имеют элегантности и простоты интерполяции.

С точки зрения реализации, `interpolate` трансформирует ваши интерполированные строки в обычные строки Haskell во время компиляции с использованием Template Haskell, так что при выполнении вашего кода не происходит потери производительности. Это умно и чисто, как и сам Haskell.

## Смотрите также

- [Hackage - пакет interpolate](https://hackage.haskell.org/package/interpolate)
- [Hackage - модуль Text.Printf](https://hackage.haskell.org/package/base/docs/Text-Printf.html)
- [Haskell Wiki - Квазикотирование](https://wiki.haskell.org/Quasiquotation)
- Для богатого шаблонизирования ознакомьтесь с [Hackage - шаблоны Mustache](https://hackage.haskell.org/package/mustache)
