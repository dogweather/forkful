---
title:                "Использование ассоциативных массивов"
date:                  2024-02-03T18:11:30.105536-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование ассоциативных массивов"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/go/using-associative-arrays.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Ассоциативные массивы, известные в Go как карты (maps), позволяют вам хранить пары ключ-значение, где каждый уникальный ключ соответствует определённому значению. Программисты используют карты для эффективного извлечения данных, их изменения и поддержки коллекции элементов, к которым можно быстро получить доступ, используя уникальные ключи.

## Как:

Создание и инициализация карты в Go может быть выполнена разными способами. Вот базовый пример, чтобы начать:

```go
package main

import "fmt"

func main() {
    // Объявление и инициализация карты
    colors := map[string]string{
        "red":   "#FF0000",
        "green": "#00FF00",
        "blue":  "#0000FF",
    }

    fmt.Println(colors)
    // Вывод: map[blue:#0000FF green:#00FF00 red:#FF0000]
}
```

Чтобы добавить или обновить элементы, вы присваиваете значение ключу так:

```go
colors["white"] = "#FFFFFF"
fmt.Println(colors)
// Вывод: map[blue:#0000FF green:#00FF00 red:#FF0000 white:#FFFFFF]
```

Доступ к значению по его ключу прост:

```go
fmt.Println("Шестнадцатеричный код красного:", colors["red"])
// Вывод: Шестнадцатеричный код красного: #FF0000
```

Чтобы удалить элемент, используйте функцию `delete`:

```go
delete(colors, "red")
fmt.Println(colors)
// Вывод: map[blue:#0000FF green:#00FF00 white:#FFFFFF]
```

Итерация по карте выполняется с использованием цикла for:

```go
for color, hex := range colors {
    fmt.Printf("Ключ: %s Значение: %s\n", color, hex)
}
```

Помните, что карты в Go неупорядочены. Порядок итерации гарантирован не может быть.

## Глубокое погружение

В Go карты реализованы как хеш-таблицы. Каждая запись в карте состоит из двух элементов: ключа и значения. Ключ хешируется для хранения записи, что позволяет выполнить операции в постоянное время для небольшого набора данных и среднюю временную сложность O(1) при правильном хешировании, которая может снижаться до O(n) в худшем случае при множестве коллизий хешей.

Важное примечание для новых программистов Go - типы карт являются ссылочными типами. Это означает, что когда вы передаете карту в функцию, любые изменения, сделанные с картой внутри этой функции, видны вызывающему. Это отличается от, скажем, передачи структуры в функцию, где структура копируется, если не передана по указателю.

Хотя карты невероятно универсальны и эффективны для большинства случаев использования ассоциативных массивов, в приложениях, где критически важна производительность, может быть полезно использовать структуры данных с более предсказуемыми характеристиками производительности, особенно если распределение ключей может вызвать частые коллизии.

Другой альтернативой стоит рассмотреть `sync.Map`, доступный с Go 1.9, предназначенный для сценариев, где ключи записываются один раз, но читаются многократно, что обеспечивает улучшение эффективности в этих сценариях. Однако для обычных приложений Go идиоматическое использование обычных карт часто является рекомендуемым подходом за счет его простоты и прямой поддержки в языке.