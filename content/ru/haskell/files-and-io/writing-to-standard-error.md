---
title:                "Запись в стандартный поток ошибок"
aliases: - /ru/haskell/writing-to-standard-error.md
date:                  2024-01-29T00:06:00.930643-07:00
model:                 gpt-4-0125-preview
simple_title:         "Запись в стандартный поток ошибок"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/haskell/writing-to-standard-error.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Запись в стандартный поток ошибок (stderr) на Haskell позволяет сообщать об ошибках и выводить информацию для отладки отдельно от стандартного вывода (stdout). Это делается для того, чтобы держать потоки вывода организованными, упрощая обработку только необходимого — например, перенаправление вывода или регистрацию ошибок.

## Как:

Используйте System.IO для записи в stderr. Вот простой пример:

```Haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "Это пойдет в stderr"
  putStrLn "Это пойдет в stdout"
```

Вывод при выполнении программы:

```
Это пойдет в stdout
```

Чтобы увидеть вывод stderr, перенаправьте его:

```bash
runhaskell your_program.hs 2> error.log
```

Теперь `error.log` содержит "Это пойдет в stderr".

## Глубокое погружение

Система ввода-вывода Haskell различает stdout и stderr, сохраняя конвенции Unix. До появления Haskell языки программирования, такие как C, задавали прецедент разделения потоков — stdout для результатов, stderr для ошибок и логов.

Альтернативные способы вывода включают использование библиотек, таких как `System.Log.Logger`, для более сложного логирования. Что касается реализации, stderr в Haskell является `Handle`, так же как и дескриптор файла, но заранее определен для ссылки на системный поток вывода ошибок.

## Смотрите также

- [Библиотека Haskell System.IO](https://hackage.haskell.org/package/base-4.16.0.0/docs/System-IO.html): Подробная документация по System.IO.
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/): Вводная книга по Haskell, которая охватывает ввод-вывод.
