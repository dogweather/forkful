---
title:                "Использование регулярных выражений"
date:                  2024-01-29T00:03:40.491819-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование регулярных выражений"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/go/using-regular-expressions.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Регулярные выражения (regex) — это шаблоны, используемые для поиска сочетаний символов в строках. Программисты используют их для поиска, проверки и манипуляций с текстом, что делает их швейцарским ножом для операций со строками.

## Как использовать:
```Go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    // Пример: поиск электронных адресов в строке
    text := "Reach out at contact@example.com or support@random.org"
    emailRegex := regexp.MustCompile(`[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}`)

    // FindString возвращает первое совпадение
    fmt.Printf("Первый email: %s\n", emailRegex.FindString(text)) 
    // Вывод: Первый email: contact@example.com

    // FindAllString возвращает все совпадения
    emails := emailRegex.FindAllString(text, -1)
    fmt.Printf("Все email: %v\n", emails) 
    // Вывод: Все email: [contact@example.com support@random.org]

    // Замена текста
    sanitizedText := emailRegex.ReplaceAllString(text, "[censored]")
    fmt.Println(sanitizedText) 
    // Вывод: Reach out at [censored] or [censored]
}
```

## Глубже
Регулярные выражения имеют корни в Unix, начиная с 1950-х годов, и получили распространение через инструменты, такие как `grep`. Позже их популяризировал Perl. Альтернативы включают использование строковых функций или парсеров для простых и структурированных данных соответственно. С точки зрения реализации, пакет `regexp` в Go основан на NFA (недетерминированный конечный автомат), что обеспечивает эффективную работу с регулярными выражениями без недостатков возврата, найденных в некоторых других движках.

## Смотри также
- Документация по пакету Go `regexp`: [pkg.go.dev/regexp](https://pkg.go.dev/regexp)
- Онлайн тестер и отладчик regex: [regex101.com](https://regex101.com/)
- Руководство по регулярным выражениям от Mozilla Developer Network: [developer.mozilla.org/ru/docs/Web/JavaScript/Guide/Regular_Expressions](https://developer.mozilla.org/ru/docs/Web/JavaScript/Guide/Regular_Expressions)
