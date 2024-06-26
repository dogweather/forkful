---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:34.066646-07:00
description: "\u041A\u0430\u043A: \u0412 Go \u0443\u0434\u0430\u043B\u0435\u043D\u0438\
  \u0435 \u0441\u0438\u043C\u0432\u043E\u043B\u043E\u0432, \u0441\u043E\u043E\u0442\
  \u0432\u0435\u0442\u0441\u0442\u0432\u0443\u044E\u0449\u0438\u0445 \u0448\u0430\u0431\
  \u043B\u043E\u043D\u0443, \u043C\u043E\u0436\u0435\u0442 \u0431\u044B\u0442\u044C\
  \ \u044D\u0444\u0444\u0435\u043A\u0442\u0438\u0432\u043D\u043E \u0432\u044B\u043F\
  \u043E\u043B\u043D\u0435\u043D\u043E \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\
  \u0437\u043E\u0432\u0430\u043D\u0438\u0435\u043C \u043F\u0430\u043A\u0435\u0442\u0430\
  \ `regexp`. \u0417\u0434\u0435\u0441\u044C \u043C\u044B \u043F\u043E\u043A\u0430\
  \u0436\u0435\u043C, \u043A\u0430\u043A \u0443\u0434\u0430\u043B\u0438\u0442\u044C\
  \ \u0432\u0441\u0435\u2026"
lastmod: '2024-03-13T22:44:44.079260-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Go \u0443\u0434\u0430\u043B\u0435\u043D\u0438\u0435 \u0441\u0438\u043C\
  \u0432\u043E\u043B\u043E\u0432, \u0441\u043E\u043E\u0442\u0432\u0435\u0442\u0441\
  \u0442\u0432\u0443\u044E\u0449\u0438\u0445 \u0448\u0430\u0431\u043B\u043E\u043D\u0443\
  , \u043C\u043E\u0436\u0435\u0442 \u0431\u044B\u0442\u044C \u044D\u0444\u0444\u0435\
  \u043A\u0442\u0438\u0432\u043D\u043E \u0432\u044B\u043F\u043E\u043B\u043D\u0435\u043D\
  \u043E \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\
  \u0438\u0435\u043C \u043F\u0430\u043A\u0435\u0442\u0430 `regexp`."
title: "\u0423\u0434\u0430\u043B\u0435\u043D\u0438\u0435 \u0441\u0438\u043C\u0432\u043E\
  \u043B\u043E\u0432, \u0441\u043E\u043E\u0442\u0432\u0435\u0442\u0441\u0442\u0432\
  \u0443\u044E\u0449\u0438\u0445 \u0448\u0430\u0431\u043B\u043E\u043D\u0443"
weight: 5
---

## Как:
В Go удаление символов, соответствующих шаблону, может быть эффективно выполнено с использованием пакета `regexp`. Здесь мы покажем, как удалить все цифры, а затем все не-алфавитно-цифровые символы из строки в качестве примеров.

1. **Удаление всех цифр:**

```go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    text := "Go1 круто, но Go2 будет круче! Сейчас: 2023."
	
    // Компилируем регулярное выражение для цифр
    re, err := regexp.Compile("[0-9]+")
    if err != nil {
        fmt.Println("Ошибка при компиляции регулярного выражения:", err)
        return
    }
	
    // Заменяем цифры на пустую строку
    result := re.ReplaceAllString(text, "")
	
    fmt.Println(result) // Вывод: Go круто, но Go будет круче! Сейчас: .
}
```

2. **Удаление всех не-алфавитно-цифровых символов:**

```go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    text := "Go номер 1 среди языков программирования!"
	
    // Компилируем регулярное выражение для не-алфавитно-цифровых символов
    re, err := regexp.Compile("[^a-zA-Z0-9]+")
    if err != nil {
        fmt.Println("Ошибка при компиляции регулярного выражения:", err)
        return
    }
	
    // Заменяем не-алфавитно-цифровые символы на пустую строку
    result := re.ReplaceAllString(text, "")
	
    fmt.Println(result) // Вывод: Goномер1средиязыковпрограммирования
}
```

## Погружение
Пакет `regexp` в Go предоставляет мощный интерфейс для сопоставления с образцом и манипуляции с регулярными выражениями. Его реализация основана на RE2, библиотеке регулярных выражений, разработанной для гарантии линейного времени выполнения, избегая возможности проблем "катастрофического возврата" (catastrophic backtracking), присутствующих в некоторых других движках регулярных выражений. Это делает регулярные выражения Go относительно безопасными и эффективными для широкого спектра приложений.

Хотя пакет `regexp` является комплексным решением для работы с шаблонами, стоит отметить, что для более простых или высокоспецифичных манипуляций со строками другие функции строки, такие как `strings.Replace()`, `strings.Trim()` или срезы, могут предложить более производительные альтернативы. Регулярные выражения — мощный инструмент, но из-за их относительной вычислительной затратности для операций, которые могут быть специфицированы без их использования, изучение альтернатив из стандартной библиотеки иногда может привести к более простому и эффективному коду.
