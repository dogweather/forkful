---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:42.298380-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 Go \u0438\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\
  \u0438\u044F \u0441\u0442\u0440\u043E\u043A \u043E\u0431\u044B\u0447\u043D\u043E\
  \ \u0434\u043E\u0441\u0442\u0438\u0433\u0430\u0435\u0442\u0441\u044F \u0441 \u0438\
  \u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\u043C \u043F\
  \u0430\u043A\u0435\u0442\u0430 `fmt`, \u0432 \u0447\u0430\u0441\u0442\u043D\u043E\
  \u0441\u0442\u0438, \u0444\u0443\u043D\u043A\u0446\u0438\u0438 `Sprintf`, \u043A\
  \u043E\u0442\u043E\u0440\u0430\u044F \u043F\u043E\u0437\u0432\u043E\u043B\u044F\u0435\
  \u0442 \u0432\u043D\u0435\u0434\u0440\u044F\u0442\u044C\u2026"
lastmod: '2024-03-13T22:44:44.083012-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Go \u0438\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0438\
  \u044F \u0441\u0442\u0440\u043E\u043A \u043E\u0431\u044B\u0447\u043D\u043E \u0434\
  \u043E\u0441\u0442\u0438\u0433\u0430\u0435\u0442\u0441\u044F \u0441 \u0438\u0441\
  \u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\u043C \u043F\u0430\
  \u043A\u0435\u0442\u0430 `fmt`, \u0432 \u0447\u0430\u0441\u0442\u043D\u043E\u0441\
  \u0442\u0438, \u0444\u0443\u043D\u043A\u0446\u0438\u0438 `Sprintf`, \u043A\u043E\
  \u0442\u043E\u0440\u0430\u044F \u043F\u043E\u0437\u0432\u043E\u043B\u044F\u0435\u0442\
  \ \u0432\u043D\u0435\u0434\u0440\u044F\u0442\u044C \u043F\u0435\u0440\u0435\u043C\
  \u0435\u043D\u043D\u044B\u0435 \u0432 \u0441\u0442\u0440\u043E\u043A\u0443, \u0443\
  \u043A\u0430\u0437\u044B\u0432\u0430\u044F \u0444\u043E\u0440\u043C\u0430\u0442\u0438\
  \u0440\u0443\u044E\u0449\u0438\u0435 \u0433\u043B\u0430\u0433\u043E\u043B\u044B."
title: "\u0418\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0438\u044F \u0441\
  \u0442\u0440\u043E\u043A\u0438"
weight: 8
---

## Как это сделать:
В Go интерполяция строк обычно достигается с использованием пакета `fmt`, в частности, функции `Sprintf`, которая позволяет внедрять переменные в строку, указывая форматирующие глаголы. Глаголы являются заполнителями в форматируемой строке и заменяются значениями заданных переменных. Вот как это работает:

```go
package main

import (
    "fmt"
)

func main() {
    name := "Jane"
    age := 28

    // Использование Sprintf для интерполяции строк
    message := fmt.Sprintf("Привет, меня зовут %s, и мне %d лет.", name, age)
    fmt.Println(message) // Вывод: Привет, меня зовут Jane, и мне 28 лет.
}
```

Обратите внимание, что `%s` используется для строк, а `%d` для целых чисел. Документация пакета `fmt` предоставляет полный список форматирующих глаголов для различных типов данных.

## Подробнее
Концепция интерполяции строк существует во многих языках программирования, хотя и с различными синтаксисами и возможностями. В Go, хотя функция `Sprintf` из пакета `fmt` является наиболее часто используемым подходом, она может не всегда быть наиболее эффективной, особенно для простых конкатенаций или при работе в коде с высокой чувствительностью к производительности.

Пакет `fmt` использует рефлексию для динамической интерпретации типов переменных во время выполнения, что, хотя и является гибким, влечет за собой накладные расходы. В сценариях, где производительность критична, прямая конкатенация строк или тип `strings.Builder` могут предложить лучшие альтернативы. Прямая конкатенация проста, но может стать громоздкой при наличии множества переменных. `strings.Builder`, с другой стороны, предоставляет более производительный и читаемый способ построения сложных строк в цикле или при работе с множеством переменных:

```go
var sb strings.Builder
sb.WriteString("Привет, меня зовут ")
sb.WriteString(name)
sb.WriteString(", и мне ")
sb.WriteString(strconv.Itoa(age))
sb.WriteString(" лет.")
message := sb.String()

fmt.Println(message) // Выводит то же самое, что и раньше
```

В конечном итоге, выбор между `fmt.Sprintf`, прямой конкатенацией и `strings.Builder` зависит от конкретных требований вашего приложения, таких как сложность создаваемой строки и соображения производительности.
