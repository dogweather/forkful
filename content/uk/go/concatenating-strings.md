---
title:                "Go: З'єднання рядків"
simple_title:         "З'єднання рядків"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/concatenating-strings.md"
---

{{< edit_this_page >}}

# Чому?

У програмуванні існує багато ситуацій, коли нам потрібно з'єднати декілька рядків тексту в один. Наприклад, для форматування повідомлень користувачам або створення URL-адрес за певним шаблоном. Використання функції "Concat" дозволяє нам швидко і просто об'єднати рядки.

# Як?

```Go
package main

import "fmt"

func main() {
    first := "Привіт"
    last := "Джон"
    full := first + " " + last
    fmt.Println(full)
}
```

Вивід: "Привіт Джон"

# Занурення

У Go існує кілька способів з'єднання рядків, таких як використання функції "Concat", використання оператора "+=" або використання пакету "strings" і його функції "Join". Також, варто знати, що у Go рядки є не змінними типами даних і тому при кожному операторі "+" або "-=" створюється новий об'єкт рядка.

# Дивись також

- [Документація Go: функція "Concat"](https://golang.org/pkg/strings/#Concat)
- [Стаття на Medium про з'єднання рядків у Go](https://medium.com/@teivah/concatenation-of-strings-in-go-227bec3f807f)
- [Розділ про рядки в офіційному Go туторіалі](https://tour.golang.org/basics/3)