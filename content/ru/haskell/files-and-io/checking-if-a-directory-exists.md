---
title:                "Проверка существования директории"
aliases:
- /ru/haskell/checking-if-a-directory-exists.md
date:                  2024-01-28T23:55:26.762479-07:00
model:                 gpt-4-0125-preview
simple_title:         "Проверка существования директории"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/haskell/checking-if-a-directory-exists.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Проверка существования директории означает подтверждение того, что папка действительно находится в файловой системе там, где вы думаете. Программисты делают это, чтобы избежать ошибок, например, попытки чтения из несуществующего каталога или непреднамеренного создания дубликатов папок.

## Как это сделать:
Haskell использует пакет `directory` для взаимодействия с файловой системой. Установите его командой `cabal install directory`, если вы еще этого не сделали. Вот как проверить наличие директории:

```Haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
    let dir = "path/to/your/directory"
    exists <- doesDirectoryExist dir
    putStrLn $ "Существует ли директория? " ++ show exists
```

Если `dir` существует, ваш вывод будет:

```
Существует ли директория? True
```

В противном случае он покажет:

```
Существует ли директория? False
```

## Подробнее
В прошлом вы, возможно, напрямую работали с системными вызовами или использовали библиотеки, менее абстрактные, чем `directory`. Теперь этот пакет Haskell выполняет всю тяжелую работу.

Альтернативы? Вы могли бы использовать более низкоуровневые операции из пакета `unix`, вызывать команды оболочки или писать свои собственные привязки FFI. Все это излишне для такой простой проверки.

Под капотом, `doesDirectoryExist` использует системно-специфичные вызовы для проверки наличия директории без генерации исключения. Это действие ввода-вывода, отсюда и необходимость функции `main` и `IO ()`.

## Смотрите также
Другие ресурсы, которые стоит рассмотреть:

- Ваша локальная документация по Haskell: `file:///usr/share/doc/ghc/html/libraries/directory/System-Directory.html`
- Hackage для пакета `directory`: [https://hackage.haskell.org/package/directory](https://hackage.haskell.org/package/directory)
