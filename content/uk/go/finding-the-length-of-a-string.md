---
title:                "Go: Знаходження довжини рядка"
simple_title:         "Знаходження довжини рядка"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Чому

Знаходження довжини рядка є важливою частиною будь-якої програми, оскільки він дозволяє отримати інформацію про кількість символів в рядку. Це необхідно для багатьох операцій в програмуванні, таких як обробка тексту, перевірка меж та здійснення дій з рядком.

## Як

Для початку, нам потрібно використовувати функцію `len`, яка дозволяє знайти довжину будь-якого рядка в Go. Нижче наведений приклад коду з використанням цієї функції:

```Go
package main

import "fmt"

func main() {
    str := "Привіт, світ!"
    length := len(str)
    fmt.Println(length) // виведе 13
}
```

Ви також можете використовувати цю функцію для знаходження довжини будь-якого виразу, як показано нижче:

```Go
package main

import "fmt"

func main() {
    num := 42
    length := len(fmt.Sprintf("%v", num)) // перетворює число в рядок та знаходить його довжину
    fmt.Println(length) // виведе 2
}
```

## Глибоке вивчення

Функція `len` використовує внутрішній тип `string`, щоб знаходити довжину рядка. Вона також автоматично враховує многобайтові символи, тому результат буде правильним навіть для рядків, що містять такі символи.

Окрім використання функції `len`, також можна отримати довжину рядка, перетворивши його в масив байтів за допомогою функції `[]byte`. Нижче наведений приклад коду:

```Go
package main

import (
    "fmt"
    "unicode/utf8"
)

func main() {
    str := "Hello, 世界"
    length := utf8.RuneCountInString(str)
    fmt.Println(length) // виведе 7
}
```

## Див. також

- [Офіційна документація Go](https://golang.org/ref/spec#Length_and_capacity)
- [Довжина строк в Go](https://www.calhoun.io/how-to-find-the-length-of-a-string-in-go/)
- [Строки та символи в Go](https://blog.golang.org/strings)