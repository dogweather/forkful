---
title:                "Виділення підрядків"
date:                  2024-01-20T17:45:49.007108-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виділення підрядків"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Витягування підрядків зі змінних типу string - це про процес отримання частин інформації з довших текстів. Програмісти роблять це для аналізу, пошуку даних, валідації або зміни формату інфо. 

## How to: (Як це зробити:)
Go дозволяє маніпулювати рядками без зайвих зусиль. Ось як можна витягнути підрядки:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	text := "Привіт, як справи?"
	
	// Витягнути все з п'ятого символу
	substring1 := text[7:]
	fmt.Println(substring1)  // вивід: як справи?

	// Витягнути від початку до знака питання
	substring2 := text[:len(text)-1]
	fmt.Println(substring2)  // вивід: Привіт, як справи

	// Використовуємо strings paket для роботи з UTF-8 string
	runes := []rune(text)
	// Витягнути слово "Привіт"
	hello := string(runes[0:6])
	fmt.Println(hello)  // вивід: Привіт
}
```

## Deep Dive (Поглиблений Підхід)
Витягнути підрядок в Go можна просто, але треба бути обережним з UTF-8 строки. Go працює з `byte` за замовчуванням, і символи за межами ASCII мають більше ніж один `byte`. Тому, ми користуємось `[]rune` для правильної обробки Unicode. Історично, це було джерелом багів, так що обережність важлива.

Є й інші способи: можна користуватися `strings` пакетом, кишеньковими функціями. Але пряма робота з `slice` часто ефективніша і простіша, особливо якщо вже знаєте місце розрізу. Пам'ятайте про ефективність при великих даних.

## See Also (Дивись Також)
Додаткова інформація та докладніше про стрінги в Go:

- Офіційна документація по пакету strings: [pkg.go.dev/strings](https://pkg.go.dev/strings)
- Go Blog про стрінги: [blog.golang.org/strings](https://blog.golang.org/strings)
- Руни і Unicode в Go: [blog.golang.org/normalization](https://blog.golang.org/normalization)
