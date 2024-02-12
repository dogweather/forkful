---
title:                "Чтение аргументов командной строки"
aliases:
- /ru/haskell/reading-command-line-arguments.md
date:                  2024-01-29T00:01:00.683963-07:00
model:                 gpt-4-0125-preview
simple_title:         "Чтение аргументов командной строки"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/haskell/reading-command-line-arguments.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Чтение аргументов командной строки на Haskell позволяет захватить пользовательский ввод при запуске вашей программы. Зачем? Чтобы на лету настраивать поведение программы, не изменяя сам код.

## Как это сделать:

```haskell
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Привет, " ++ show args ++ "!")
```

Запустите, передав "world" в качестве аргумента:

```bash
$ runhaskell yourprogram.hs world
Привет, ["world"]!
```

## Погружение

Haskell - аккуратный язык, уходящий корнями в 80-е, предпочитающий чистоту и статическую типизацию. Уже с давних времен в нем были способы обработки аргументов командной строки. В других языках это могло бы быть довольно процедурным делом, но здесь мы имеем дело с монадами IO для взаимодействия с внешним миром.

Альтернативы? Можно поиграться с библиотеками вроде `optparse-applicative` для сложных случаев, но для простых случаев `getArgs` справляется с задачей.

Что под капотом? `getArgs` — это функция, которая погружается в вашу систему, извлекает все, что следует за именем программы в терминале, и предоставляет вам список строк. Это реализовано в базовой библиотеке Haskell, опираясь на функции на уровне C для выполнения основной работы. Круто, правда?

## Смотрите также

- Больше о `getArgs`: [Hoogle по System.Environment](https://hoogle.haskell.org/?hoogle=System.Environment.getArgs)
- Углубление в парсинг аргументов: [optparse-applicative на Hackage](https://hackage.haskell.org/package/optparse-applicative)
