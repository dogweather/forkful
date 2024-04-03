---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:06:00.930643-07:00
description: "\u041A\u0430\u043A: \u0418\u0441\u043F\u043E\u043B\u044C\u0437\u0443\
  \u0439\u0442\u0435 System.IO \u0434\u043B\u044F \u0437\u0430\u043F\u0438\u0441\u0438\
  \ \u0432 stderr. \u0412\u043E\u0442 \u043F\u0440\u043E\u0441\u0442\u043E\u0439 \u043F\
  \u0440\u0438\u043C\u0435\u0440."
lastmod: '2024-03-13T22:44:45.163531-06:00'
model: gpt-4-0125-preview
summary: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0439\u0442\u0435 System.IO\
  \ \u0434\u043B\u044F \u0437\u0430\u043F\u0438\u0441\u0438 \u0432 stderr."
title: "\u0417\u0430\u043F\u0438\u0441\u044C \u0432 \u0441\u0442\u0430\u043D\u0434\
  \u0430\u0440\u0442\u043D\u044B\u0439 \u043F\u043E\u0442\u043E\u043A \u043E\u0448\
  \u0438\u0431\u043E\u043A"
weight: 25
---

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
