---
title:                "Конкатенація рядків"
html_title:           "PHP: Конкатенація рядків"
simple_title:         "Конкатенація рядків"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що це й навіщо?

Конкатенація стрічок – це процес їх об’єднання в одну. Розробники використовують її для формування і маніпулювання даними.

## Як це зробити:

Нижче наведений приклад коду на Go для конкатенації двох стрічок.

```Go
package main

import "fmt"

func main() {
	str1 := "Вітаємо"
	str2 := " в Go!"
	concatenated := str1 + str2
	fmt.Println(concatenated)
}
```

Коли виконаєте цей код, отримаєте наступний результат:

```Go 
"Вітаємо в Go!"
```

## Занурення в деталі:

**Історичний контекст:** Конкатенація стрічок — це основна техніка, яку можна знайти в більшості мов програмування, її коріння вийшло ще з часів, коли створювалися перші мови програмування.

**Альтернативи:** В Go є багато способів для конкатенації рядків. Один з них — використання функції `Sprintf` з пакету `fmt`, яка дозволяє об’єднувати рядки з форматуванням.

```Go
package main

import "fmt"

func main() {
	str1 := "Вітаємо"
	str2 := " в Go!"
	concatenated := fmt.Sprintf("%s%s", str1, str2)
	fmt.Println(concatenated)
}
```

**Деталі реалізації:** Go використовує статичну типізацію, в результаті чого операція + може використовуватися для конкатенації рядків. Коли викликається ця операція, Go автоматично виконує потрібне приведення типів.

## Дивіться також:

- [Official Go Documentation](https://golang.org/doc/)
- [Strings, bytes, runes and characters in Go](https://blog.golang.org/strings)
- [fmt package - GoDoc](https://godoc.org/fmt)