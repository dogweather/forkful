---
title:                "Перетворення рядка в нижній регістр"
html_title:           "Javascript: Перетворення рядка в нижній регістр"
simple_title:         "Перетворення рядка в нижній регістр"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що і навіщо?
Приведення рядка до нижнього регістру - це процес перетворення всіх символів УПЕРЕДЖЕНОГО рядка в НИЖНІЙ регістр. Програмісти роблять це, щоб забезпечити однорідність та сталість порівнянь, часто в межах завдань пошуку та сортування. 

## Як це зробити:
В Go приведення до нижнього регістру виконується за допомогою функції `strings.ToLower()`. Приклад:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "Hello, WOrld"
	fmt.Println(strings.ToLower(str))
}
```
Виведення:

```Go
hello, world
```

## Поглиблений розбір
Приведення до нижнього регістру - це простий, але важливий інструмент для програміста, який має глибокі коріння в історії програмування. Що стосується альтернатив, то в Go це найбільш прямий спосіб. Ви також можете використовувати регулярні вирази або власні методи, але вони, як правило, менш ефективні. Щодо деталей реалізації, `strings.ToLower()` працює, проходячи через кожний символ в рядку і змінюючи його, якщо він знаходиться в верхньому регістрі.

## Дивись також
1. Посібник з Go: [https://go.dev/playground/](https://go.dev/playground/)
2. Документація по рядках в Go: [https://pkg.go.dev/strings](https://pkg.go.dev/strings)
3. Форум Go: [https://forum.golangbridge.org/](https://forum.golangbridge.org/)