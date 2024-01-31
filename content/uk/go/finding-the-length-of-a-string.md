---
title:                "Визначення довжини рядка"
date:                  2024-01-20T17:47:28.934711-07:00
model:                 gpt-4-1106-preview
simple_title:         "Визначення довжини рядка"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Що та Чому?

Визначення довжини рядка – це процес з'ясування, скільки символів він містить. Програмісти роблять це для перевірки, обробки текстових даних, або для валідації вводу користувача.

## Як це зробити:

```Go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	str := "Привіт, світ!"
	fmt.Println("Byte length:", len(str)) // Довжина у байтах
	fmt.Println("Rune count:", utf8.RuneCountInString(str)) // Кількість кодових пунктів Unicode

	// Вивід:
	// Byte length: 20
	// Rune count: 12
}
```

## Поглиблено:

У Go, рядки є впорядкованими послідовностями байтів. Однак, з появою Unicode, обчислення довжини рядка стало складнішим. Не всі символи кодуються одним байтом. Для справжньої довжини рядка в Unicode ми використовуємо функцію `utf8.RuneCountInString()`.

Альтернативи `len()` включають пряме ітерування по рунам рядка, що може бути корисним для додаткових операцій обробки кожного символу. Однак це може бути менш продуктивним для просто визначення кількості символів.

## Дивіться також:

- Документація по рядках у Go: [Strings, bytes, runes and characters in Go](https://blog.golang.org/strings)
- Офіційна документація по пакету `unicode/utf8`: [Package utf8](https://golang.org/pkg/unicode/utf8/)
- Туторіал по рядкам і рунам у Go: [Go by Example: Strings and Runes](https://gobyexample.com/strings-and-runes)
