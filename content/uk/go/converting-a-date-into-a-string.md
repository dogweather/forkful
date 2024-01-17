---
title:                "Перетворення дати в рядок"
html_title:           "Go: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Що & Чому?
В Go, конвертування дати в рядок є процесом перетворення дати у вигляді рядка з текстового формату. Програмісти виконують цю задачу для зручності обробки та відображення дат у своїх програмах.

## Як зробити:
```Go
package main

import (
  "fmt"
  "time"
)

func main() {
  now := time.Now()
  dateStr := now.Format("January 02, 2006") // Результат: June 16, 2021
  fmt.Println(dateStr)
}
```

```Go
package main

import (
  "fmt"
  "time"
)

func main() {
  now := time.Now()
  dateStr := now.Format("02/01/06") // Результат: 16/06/21
  fmt.Println(dateStr)
}
```

```Go
package main

import (
  "fmt"
  "time"
)

func main() {
  now := time.Now()
  dateStr := now.Format("Monday, January 02") // Результат: Wednesday, June 16
  fmt.Println(dateStr)
}
```

## Глибоке занурення:
Конвертування дати в рядок датиє свої корені ще у часи використання старих мов програмування, коли дати використовувалися у числовому форматі і було складно читати та зрозуміти їх. У сучасних мовах, таких як Go, стало зручно використовувати текстові формати для дат, що полегшує розробку та зрозумілисть програмного коду. Іншими способами конвертування дати в рядок є використання спеціальних бібліотек і ручне форматування дати за допомогою функцій маніпулювання рядками.

## Дивитися також:
- [Документація Go про пакет часу](https://golang.org/pkg/time)
- [Бібліотека для створення різноманітних форматів дат (GitHub)](https://github.com/mitchellh/go-strftime)