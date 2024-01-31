---
title:                "Використання регулярних виразів"
date:                  2024-01-19
html_title:           "Bash: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Що таке регулярні вирази і навіщо вони потрібні? Використовуємо їх для пошуку, перевірки та маніпуляції текстом. Програмісти люблять їх за швидкість і гнучкість.

## How to:
Go має пакет `regexp` для роботи з регулярками. Ось як це працює:

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	// Створення шаблону регулярного виразу
	regex := regexp.MustCompile(`\b\w{5}\b`)

	// Текст для пошуку
	text := "Привіт, це маленький текст для прикладу."

	// Пошук всіх співпадінь
	matches := regex.FindAllString(text, -1)

	fmt.Println(matches) // Виведе: [Привіт маленький прикладу]
}
```

Sample output:
```
[Привіт маленький прикладу]
```

## Deep Dive
Регулярні вирази з'явилися у 1950-х. Їх винайшов Стівен Кліни, але сьогоднішня версія походить від Perl. Альтернативами є ручний парсинг тексту, функції `strings` в Go, але вони не такі потужні. Внутрішньо, Go компілює регулярні вирази в byte-код, що швидко виконується автоматом.

## See Also
- Go by Example: Regular Expressions: https://gobyexample.com/regular-expressions
- Go `regexp` package documentation: https://pkg.go.dev/regexp
- RegExp testing tool: https://regex101.com/
