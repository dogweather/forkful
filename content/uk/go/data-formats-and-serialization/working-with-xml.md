---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:40.017595-07:00
description: "\u042F\u043A: \u0414\u043B\u044F \u043F\u0430\u0440\u0441\u0438\u043D\
  \u0433\u0443 XML \u0443 Go \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\
  \u0432\u0443\u0454\u0442\u044C\u0441\u044F \u043F\u0430\u043A\u0435\u0442 `encoding/xml`.\
  \ \u0426\u0435\u0439 \u043F\u0430\u043A\u0435\u0442 \u043D\u0430\u0434\u0430\u0454\
  \ \u043D\u0435\u043E\u0431\u0445\u0456\u0434\u043D\u0456 \u0456\u043D\u0441\u0442\
  \u0440\u0443\u043C\u0435\u043D\u0442\u0438 \u0434\u043B\u044F \u0434\u0435\u043C\
  \u0430\u0440\u0448\u0430\u043B\u0456\u043D\u0433\u0443 (\u043F\u0430\u0440\u0441\
  \u0438\u043D\u0433\u0443) XML \u0443 \u0441\u0442\u0440\u0443\u043A\u0442\u0443\u0440\
  \u0438 Go.\u2026"
lastmod: '2024-03-13T22:44:48.478114-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043B\u044F \u043F\u0430\u0440\u0441\u0438\u043D\u0433\u0443 XML\
  \ \u0443 Go \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0454\
  \u0442\u044C\u0441\u044F \u043F\u0430\u043A\u0435\u0442 `encoding/xml`."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML"
weight: 40
---

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
