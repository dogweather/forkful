---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:05:27.194281-07:00
description: "\u0420\u0430\u0437\u0431\u043E\u0440 \u0434\u0430\u0442\u044B \u0438\
  \u0437 \u0441\u0442\u0440\u043E\u043A\u0438 \u043D\u0430 Go \u0432\u043A\u043B\u044E\
  \u0447\u0430\u0435\u0442 \u043F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\
  \u0430\u043D\u0438\u0435 \u0434\u0430\u0442\u044B, \u043F\u0440\u0435\u0434\u0441\
  \u0442\u0430\u0432\u043B\u0435\u043D\u043D\u043E\u0439 \u0432 \u0432\u0438\u0434\
  \u0435 \u0442\u0435\u043A\u0441\u0442\u0430, \u0432 \u0431\u043E\u043B\u0435\u0435\
  \ \u0443\u0434\u043E\u0431\u043D\u044B\u0439 \u0444\u043E\u0440\u043C\u0430\u0442\
  \ (\u043D\u0430\u043F\u0440\u0438\u043C\u0435\u0440, `time.Time`). \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B\u2026"
lastmod: '2024-03-13T22:44:44.128526-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0430\u0437\u0431\u043E\u0440 \u0434\u0430\u0442\u044B \u0438\u0437\
  \ \u0441\u0442\u0440\u043E\u043A\u0438 \u043D\u0430 Go \u0432\u043A\u043B\u044E\u0447\
  \u0430\u0435\u0442 \u043F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\
  \u043D\u0438\u0435 \u0434\u0430\u0442\u044B, \u043F\u0440\u0435\u0434\u0441\u0442\
  \u0430\u0432\u043B\u0435\u043D\u043D\u043E\u0439 \u0432 \u0432\u0438\u0434\u0435\
  \ \u0442\u0435\u043A\u0441\u0442\u0430, \u0432 \u0431\u043E\u043B\u0435\u0435 \u0443\
  \u0434\u043E\u0431\u043D\u044B\u0439 \u0444\u043E\u0440\u043C\u0430\u0442 (\u043D\
  \u0430\u043F\u0440\u0438\u043C\u0435\u0440, `time.Time`). \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B\u2026"
title: "\u0420\u0430\u0437\u0431\u043E\u0440 \u0434\u0430\u0442\u044B \u0438\u0437\
  \ \u0441\u0442\u0440\u043E\u043A\u0438"
---

{{< edit_this_page >}}

## Что и Почему?

Разбор даты из строки на Go включает преобразование даты, представленной в виде текста, в более удобный формат (например, `time.Time`). Программисты выполняют эту задачу для более точной работы с данными о дате и времени в приложениях, особенно при работе с пользовательским вводом, API или системами хранения, где даты часто представлены строками.

## Как это сделать:

Go предоставляет мощную поддержку для разбора дат и времени через пакет `time`. Ключ к пониманию - формат базовой даты Go: `Mon Jan 2 15:04:05 MST 2006`, который вы используете, чтобы сказать Go, как интерпретировать входящую строку. Вот быстрый пример, чтобы начать:

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Пример строки с датой
	dateStr := "2023-04-12 14:45:00"
	
	// Определите макет/формат входящей строки с датой
	// Этот макет говорит Go ожидать год, затем месяц, 
	// затем день, час, минуту и, наконец, секунду
	layout := "2006-01-02 15:04:05"
	
	// Разбор строки даты согласно макету
	parsedDate, err := time.Parse(layout, dateStr)
	if err != nil {
		fmt.Println("Ошибка при разборе даты:", err)
		return
	}
	
	// Вывод разобранной даты
	fmt.Println("Разобранная Дата:", parsedDate)
}
```

Когда вы запустите этот код, получите:

```
Разобранная Дата: 2023-04-12 14:45:00 +0000 UTC
```

Обратите внимание, как строка `layout` использует значения базовой даты для указания формата входной строки. Настройте `layout`, чтобы он соответствовал формату ваших входных дат.

## Глубокое погружение

Дизайн разбора дат и времени в Go уникален, используя специфическую базовую дату (`Mon Jan 2 15:04:05 MST 2006`). Этот подход, в отличие от использования более конвенциональных спецификаторов формата (например, `YYYY` для года), был выбран за его читабельность и удобство использования, применяя формат на основе примеров.

Хотя это может сначала показаться необычным программистам, привыкшим к другим языкам, многие находят его более интуитивным после короткого периода адаптации. Для приложений, требующих более сложной манипуляции с датами или форматов, не поддерживаемых напрямую пакетом `time` Go, сторонние библиотеки, такие как `github.com/jinzhu/now`, могут предложить дополнительную функциональность. Однако для большинства стандартных приложений встроенные возможности Go являются надежными, производительными и идиоматичными, воплощая философию Go простоты и ясности.
