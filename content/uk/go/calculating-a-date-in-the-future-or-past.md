---
title:                "Обчислення дати у майбутньому або минулому"
date:                  2024-01-20T17:31:05.491601-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обчислення дати у майбутньому або минулому"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Що і чому?
Розрахунок дати у майбутньому чи минулому потрібен, щоб маніпулювати та порівнювати часові періоди. Програмісти виконують це для планування, нагадувань, аналітики чи перевірок.

## Як це зробити:
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	now := time.Now()
	fmt.Println("Сьогодні:", now.Format("02-01-2006"))

	// Додавання двох тижнів до поточної дати
	twoWeeksLater := now.Add(time.Hour * 24 * 14)
	fmt.Println("Дві неділі після:", twoWeeksLater.Format("02-01-2006"))

	// Віднімання одного місяця з поточної дати
	oneMonthBefore := now.AddDate(0, -1, 0)
	fmt.Println("Місяць назад:", oneMonthBefore.Format("02-01-2006"))
}
```
Sample output:
```
Сьогодні: 23-03-2023
Дві неділі після: 06-04-2023
Місяць назад: 23-02-2023
```

## Глибше занурення
У мові програмування Go для маніпуляцій з датою та часом використовують пакет `time`. Він простий, але могутній і з'явився у мові з її появою. Інші мови, як PHP чи Python, мають свої засоби, але `time` в Go вважається одним з найзручніших для розуміння та використання. Метод `Add` дозволяє додати або відняти період, а `AddDate` - маніпулювати роками, місяцями та днями. Перевагою є те, що Go автоматично обраховує переходи між місяцями, високосними роками тощо.

## Див. також
- Офіційні документи по пакету `time`: https://golang.org/pkg/time/
- Go by Example - Date and Time: https://gobyexample.com/time
- Ця стаття на Go Tour: https://tour.golang.org/welcome/4
