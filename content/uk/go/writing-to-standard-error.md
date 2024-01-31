---
title:                "Запис в стандартний потік помилок"
date:                  2024-01-19
html_title:           "Arduino: Запис в стандартний потік помилок"
simple_title:         "Запис в стандартний потік помилок"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Що це та навіщо?
Стандартний потік помилок (stderr) - це канал, куди додатки виводять повідомлення про помилки. Перенаправлення помилок в stderr дає змогу легко відокремити їх від звичайного виводу програми.

## Як це зробити:
```go
package main

import (
    "fmt"
    "os"
)

func main() {
    if _, err := fmt.Fprintf(os.Stderr, "Помилка: нещось пішло не так\n"); err != nil {
        panic(err)
    }
}
```
Вивід:
```
Помилка: нещось пішло не так
```

## Поглиблено:
Перші версії Unix використовували stderr для діагностики. `os.Stderr` у Go - це файловий дескриптор, еквівалентний stderr у системному виклику. Альтернативи: логування й запусками скриптів, але stderr — стандарт для помилок. Go реалізовує це через пакет os, який надає интерфейс до файлової системи операційної системи.

## Дивіться також:
- Документація Go по пакету `os`: https://pkg.go.dev/os
- UNIX стандартні потоки: https://en.wikipedia.org/wiki/Standard_streams
- Працювання з файлами в Go: https://gobyexample.com/writing-files
