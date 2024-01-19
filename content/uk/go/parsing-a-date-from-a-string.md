---
title:                "Аналіз дати з рядка"
html_title:           "C++: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?

Аналіз дати з рядка - це процес, під час якого ми перетворюємо текстове представлення дати в формі рядка в конкретний формат дати, яким машина може маніпулювати.	  Програмісти роблять це для кращої обробки і маніпуляцій датами в їх коді.

## Як це зробити:
 
В Go програмуванні, це можна зробити використовуючи пакет `time` і метод `Parse`.

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	layout := "2006-01-02"
	str := "2020-08-31"
	t, err := time.Parse(layout, str)
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println(t)
}
```

Якщо ви запустите цей код, отримаєте наступний вивід:

```Go
2020-08-31 00:00:00 +0000 UTC
```

## Пірнання в глибину

1. Історичний контекст: Дати представлені в рядках часто не мають стандартного формату, це може бути будь-який варіянт від "31/08/2020" до "31st August, 2020". Відповідно, нам необхідно зрозуміти як цей конкретний рядок має бути перетворений у дату, і саме тут приходить на допомогу парсинг дат.

2. Альтернативи: В Go існують інші методи та бібліотеки, що можуть використовуватися для парсингу дат, наприклад jodaTime або dateparse.

3. Деталі реалізації: Важливо розуміти, що рядок `layout`, який ви передаєте в метод `Parse` - це шаблон. Він використовує конкретний момент часу (Mon Jan 2 15:04:05 MST 2006) для визначення формату вхідного рядка.

## Дивіться також

1. [Go Time Package Documentation](https://golang.org/pkg/time/)
2. [Go by Example: Time Formatting / Parsing](https://gobyexample.com/time-formatting-parsing)
3. [Go Date Parsing](https://programming.guide/go/format-parse-string-time-date-example.html)