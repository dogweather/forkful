---
title:                "Порівняння двох дат"
html_title:           "Clojure: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що і чому?

Порівняння двох дат - це процес визначення, яка дата є ранішою, пізнішою або чи дати однакові. Програмісти роблять це для обробки подій у виправленому часі, сортування дат та роботи з термінами дії різних речей.

## Як це робити:

```Go 
package main

import (
	"fmt"
	"time"
)

func main() {
	date1 := time.Date(2000, 1, 1, 0, 0, 0, 0, time.UTC)
	date2 := time.Date(2000, 1, 2, 0, 0, 0, 0, time.UTC)

	if date1.Before(date2) {
		fmt.Println("Date1 is before date2")
	}

	if date1.After(date2) {
		fmt.Println("Date1 is after date2")
	}

	if date1.Equal(date2) {
		fmt.Println("Date1 is equal to date2")
	}
}
```
Примітка: Використовуйте методи `Before`, `After` та `Equal` для порівняння дат.

## Поглиблений погляд:

Функції `Before`, `After` та `Equal` були впроваджені в Go для простого порівняння часу та дат. Вони повертають `true` або `false` в залежності від того, чи є одна дата перед, після або рівною іншої. Замість цього, могло б бути використано різницю між двома датами, але це менш очевидний і прямий спосіб. Вони реалізовані через порівняння Unix часових міток обох дат, що забезпечує точність і ефективність.

## Дивіться також:

1. Офіційна документація Go про пакет Time: [https://golang.org/pkg/time/](https://golang.org/pkg/time/)
2. Інтерактивний вступ до мови програмування Go: [https://tour.golang.org/welcome/1](https://tour.golang.org/welcome/1)
3. Go Time пакет - занурення в глибини: [https://medium.com/@diwakarvats/go-time-package-deep-dive-5c02ac10901d](https://medium.com/@diwakarvats/go-time-package-deep-dive-5c02ac10901d)