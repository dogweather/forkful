---
title:                "Перетворення дати в рядок"
date:                  2024-01-20T17:36:56.803081-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення дати в рядок"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Що і чому?)
Перетворення дати в рядок дозволяє нам зручно зберігати та відображати часові мітки. Програмісти роблять це для локалізації, логування, інтерфейсів користувача, і коли треба використати дату як частину текстового формату.

## How to: (Як це зробити:)
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Створення об'єкта часу
	currentTime := time.Now()
	
	// Форматування дати у рядок
	dateStr := currentTime.Format("2006-01-02 15:04:05")
	
	// Виведення рядка
	fmt.Println("Дата та час у форматі рядка:", dateStr)
}
```
Sample output:
```
Дата та час у форматі рядка: 2023-04-07 15:21:34
```

## Deep Dive (Поглиблений аналіз):
У Go, стандартний підхід до перетворення дати в рядок — використання методу `Format` з пакету `time`. Цікаво також про "магічну дату" у Go: "2006-01-02 15:04:05". Це не просто формат, а скоріше шаблон, де кожен компонент часу представлений його реальним значенням з референтної дати — часу коли Google почав працювати над Go.

Є альтернативи за замовчуванням, наприклад `String` метод типу `Time`, але він дає менше контролю над форматом. Стандартизація обробки часу й дати — це ключ до міжнародних аплікацій та систем, які спілкуються одне з одним.

## See Also (Дивіться також):
- Офіційна документація по пакету `time`: https://golang.org/pkg/time/
- Знайомство з Go датами і часом: https://yourbasic.org/golang/format-parse-string-time-date-example/
- `time.Format` layout reference: https://gobyexample.com/time-formatting-parsing