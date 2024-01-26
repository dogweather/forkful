---
title:                "Пошук та заміна тексту"
date:                  2024-01-20T17:57:49.531873-07:00
model:                 gpt-4-1106-preview
simple_title:         "Пошук та заміна тексту"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Що і Чому?
Текстовий пошук і заміна — це процес знаходження рядків за патерном і їх заміни на інший текст. Програмісти використовують це для редагування коду, автоматизації змін у документації та оптимізації даних.

## Як це зробити:
```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	originalText := "Hello, World! Go is awesome."
	searchFor := "World"
	replaceWith := "Ukraine"
	newText := strings.ReplaceAll(originalText, searchFor, replaceWith)

	fmt.Println(newText) // Вивод: Hello, Ukraine! Go is awesome.
}
```
## Поглиблене Вивчення:
Історично пошук і заміна тексту бере свої корені в редагуванні текстів і ранні події програмування, де обробка тексту була ключовою задачею. В Go, для таких завдань часто використовується пакет `strings`. Альтернативою є регулярні вирази з пакету `regexp`, які дозволяють здійснювати більш складні операції пошуку і заміни.

## Ознайомтеся також:
- [Пакет strings](https://pkg.go.dev/strings)
- [Пакет regexp](https://pkg.go.dev/regexp)
