---
title:                "Видалення символів, що відповідають патерну"
html_title:           "C: Видалення символів, що відповідають патерну"
simple_title:         "Видалення символів, що відповідають патерну"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Що & Навіщо?
Видалення символів за шаблоном - це процес позбуття від окремих символів на основі певних критеріїв. Це часто використовується у програмуванні для очищення вводу або форматування даних.

## Як це зробити:
Ось приклад коду Go, який видаляє всі цифри зі стрічки:
```Go
package main

import (
	"fmt"
	"strings"
	"unicode"
)

func main() {
	str := "abc123def456"
	fmt.Println(str)

	result := strings.Map(func(r rune) rune {
		if unicode.IsDigit(r) {
			return -1
		}
		// return the rune if it's not a digit
		return r
	}, str)

	fmt.Println(result)  
}
```
При виконанні цього коду виведе наступне:
```
abc123def456
abcdef
```

## Поглиблений аналіз
Видалення символів за шаблоном - це загальна концепція, яка присутня не тільки в Go. Це важлива техніка, яка була використана у багатьох мовах програмування з часів C і Perl.

Є кілька альтернатив вбудованому методу strings.Map(). Одним з них є regex.ReplaceAllStringFunc(), який пропонує більше гнучкості, але може бути повільнішим у виконанні.

Неважливо, який метод вибрати, важливо знати, що видалення символів за шаблоном відбувається через генерацію нової стрічки, а не модифікацію оригіналу. 

## Див. також
- [Стрічки в Go](https://blog.golang.org/strings)
- [Розуміння Unicode в Go](https://blog.golang.org/strings)
- [Документація Go по strings](https://golang.org/pkg/strings/)