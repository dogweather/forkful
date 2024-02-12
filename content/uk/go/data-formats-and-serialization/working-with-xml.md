---
title:                "Робота з XML"
date:                  2024-02-03T18:13:40.017595-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з XML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/working-with-xml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Робота з XML у Go включає парсинг (читання) та генерацію (запис) XML документів — стандартний формат для структурованого обміну даними. Програмісти роблять це для зберігання даних, налаштувань конфігурації або обміну даними між системами, особливо в середовищах, де XML є вподобаним або застарілим форматом даних.

## Як:

### Парсинг XML у Go
Для парсингу XML у Go використовується пакет `encoding/xml`. Цей пакет надає необхідні інструменти для демаршалінгу (парсингу) XML у структури Go. Наприклад, розгляньте наступні XML дані, що представляють книгу:

```xml
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

Для парсингу цього, визначте структуру, яка відображає структуру XML:

```go
package main

import (
    "encoding/xml"
    "fmt"
    "os"
)

type Book struct {
    XMLName xml.Name `xml:"book"`
    ID      string   `xml:"id,attr"`
    Title   string   `xml:"title"`
    Author  string   `xml:"author"`
    Pages   int      `xml:"pages"`
}

func main() {
    data := []byte(`
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
`)

    var book Book
    err := xml.Unmarshal(data, &book)
    if err != nil {
        panic(err)
    }

    fmt.Printf("Book: %+v\n", book)
}
```

Вивід:

```
Book: {XMLName:{Space: Local:book} ID:123 Title:Learning Go Author:John Doe Pages:359}
```

### Генерація XML у Go
Для генерації XML документа зі структур даних Go, знову використовується пакет `encoding/xml`. На цей раз ви маршалите структури Go у XML. Враховуючи попередню структуру `Book`:

```go
package main

import (
    "encoding/xml"
    "fmt"
    "os"
)

func main() {
    book := &Book{
        ID:     "123",
        Title:  "Learning Go",
        Author: "John Doe",
        Pages:  359,
    }

    output, err := xml.MarshalIndent(book, "", "    ")
    if err != nil {
        panic(err)
    }

    fmt.Println(xml.Header + string(output))
}
```

Вивід:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

## Поглиблений аналіз

Надмірність та складність XML призвели до популярності JSON та інших форматів для багатьох застосувань. Однак, здатність XML представляти складні ієрархічні дані та його широке використання в застарілих системах та конкретних доменах (наприклад, SOAP сервіси) забезпечують його актуальність.

Пакет `encoding/xml` у Go надає потужні механізми для роботи з XML, але варто зазначити його обмеження. Наприклад, обробка просторів імен XML може бути обтяжливою та може вимагати більш детального розуміння специфікації XML, ніж для простіших випадків використання. Крім того, хоча статична типізація в Go і можливості маршалінгу та демаршалінгу пакета `encoding/xml` загалом є ефективними, розробники можуть зіткнутися з викликами при роботі з глибоко вкладеними структурами або при обробці XML документів, які не чітко відображаються на систему типів Go.

Для більшості сучасних застосувань альтернативи, такі як JSON, є простішими та ефективнішими. Однак, коли працюєте в контекстах, що вимагають XML — через застарілі системи, конкретні стандарти галузі або потреби в представленні складних даних — стандартна бібліотека Go надає надійні інструменти для виконання завдання. Як завжди, найкращий вибір формату даних залежить від конкретних вимог до застосунку та середовища.
