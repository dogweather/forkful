---
title:                "Видалення символів за візерунком"
date:                  2024-01-20T17:42:26.627043-07:00
model:                 gpt-4-1106-preview
simple_title:         "Видалення символів за візерунком"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (Що і чому?)
Видалення символів, що відповідають патерну, це процес фільтрації рядка за певними правилами. Програмісти це роблять для очистки тексту, валідації вводу користувача або підготовки даних для обробки.

## How to: (Як це зробити:)
```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	originalString := "Hello, друзі! This is a 123 test."
	pattern := "[0-9]+"  // Видалення усіх цифр

	re := regexp.MustCompile(pattern)
	cleanedString := re.ReplaceAllString(originalString, "")

	fmt.Println("Before:", originalString)
	fmt.Println("After: ", cleanedString)
}
```

Sample output:
```
Before: Hello, друзі! This is a 123 test.
After:  Hello, друзі! This is a  test.
```

## Deep Dive (Поглиблений аналіз):
Видалення символів з рядка — класична проблема. Виникла разом з потребою обробки тексту. Регулярні вирази (regex) — потужний механізм для цього з 1950-х.

Альтернативи regex включають видалення символів через цикли або використання функцій обробки рядків, проте regex дає більше гнучкості.

В Go робота з regex відбувається через пакет `regexp`. Важливо використовувати `MustCompile` для валідації патернів під час компіляції. Метод `ReplaceAllString` використовується для заміни.

## See Also (Дивіться також):
- [Package regexp](https://pkg.go.dev/regexp)
- [Go by Example: Regular Expressions](https://gobyexample.com/regular-expressions)
- [Regular Expressions: a complete guide](https://www.regular-expressions.info/tutorial.html) (може бути англомовним ресурсом)
- [A Tour of Go - Regular Expressions](https://tour.golang.org/moretypes/19)
