---
title:    "Go: Застосування великих літер до рядка"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Чому

В програмуванні мовою Go часто виникає потреба написати функцію для зміни регістру символів у рядку. Це необхідно для багатьох завдань, наприклад, для перетворення заголовків у великі літери або для правильного форматування вхідних даних.

## Як

У мові Go є вбудована функція `strings.ToUpper()`, яка дозволяє перетворити всі символи у верхній регістр у заданому рядку. Наприклад:

```Go
package main

import (
  "fmt"
  "strings"
)

func main() {
  str := "привіт, світ!"
  fmt.Println(strings.ToUpper(str))
}

// Вивід: ПРИВІТ, СВІТ!
```

## Глибоке вивчення

Якщо вам потрібна більш гнучка функціональність для зміни регістру символів, ви можете використовувати пакет `unicode` та його функції `ToUpper()` та `ToLower()`. Ці функції мають додаткові опції для роботи з тим, які символи варто перетворювати або які не варто. Наприклад:

```Go
package main

import (
  "fmt"
  "unicode"
)

func main() {
  str := "Привіт, Світ!"
  fmt.Println(strings.Map(unicode.ToUpper, str))
}

// Вивід: ПРИВІТ, СВІТ!
```

## Дивіться також

- [Документація по пакету `strings` у Go](https://golang.org/pkg/strings/)
- [Документація по пакету `unicode` у Go](https://golang.org/pkg/unicode/)
- [Приклади використання `strings.ToUpper()`](https://www.dotnetperls.com/uppercase-go)