---
title:                "Запис в стандартний потік помилок"
html_title:           "Arduino: Запис в стандартний потік помилок"
simple_title:         "Запис в стандартний потік помилок"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Що таке запис на стандартний потік помилок і навіщо це робити? Програмісти використовують стандартний потік помилок (stderr) для ізоляції повідомлень про помилки від звичайного виводу програми. Це дозволяє зберігати помилки та відслідковувати їх окремо.

## How to:

В Haskell запис на `stderr` можна виконати за допомогою функції `hPutStr` з модуля `System.IO`, вказавши `stderr` як цільовий дескриптор.

```haskell
import System.IO

main :: IO ()
main = do
    hPutStr stderr "Це повідомлення помилки\n"
```

Виведе:
```
Це повідомлення помилки
```

## Deep Dive

У UNIX-подібних системах stderr був запропонований як частина концепції потоків вводу/виводу. Це дозволяє направляти повідомлення про помилки в окремі файли або потоки без змішування з основним виводом.
Альтернативи стандартному потоку помилок в Haskell — це використання логування із застосуванням спеціальних бібліотек. Імплементація запису на stderr нічим не відрізняється від запису на стандартний вивід (stdout), всього лише потрібно обрати відповідний дескриптор файлу.

## See Also

- Haskell Documentation for the `System.IO` library: http://hackage.haskell.org/package/base-4.16.1.0/docs/System-IO.html
- The Haskell 2010 Language Report for more on input/output: https://www.haskell.org/onlinereport/haskell2010/haskellch7.html
- For historical context and a deeper understanding of standard streams: https://en.wikipedia.org/wiki/Standard_streams