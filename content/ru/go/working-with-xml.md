---
title:                "Работа с XML"
date:                  2024-01-29T00:04:54.169231-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с XML"

category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/go/working-with-xml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Работа с XML включает в себя разбор, создание и манипулирование XML-документами с помощью кода. Программисты делают это для обмена данными, файлов конфигурации и веб-сервисов, потому что читаемость XML и широкая поддержка делают его надёжным выбором для структурированных данных.

## Как:
В Go используйте пакет `encoding/xml`. Давайте разберемся и сгенерируем XML.
```go
package main

import (
	"encoding/xml"
	"fmt"
	"os"
)

// Структуры отображаются в элементы XML
type Plant struct {
	XMLName xml.Name `xml:"plant"`
	Id      int      `xml:"id,attr"`
	Name    string   `xml:"name"`
	Origin  []string `xml:"origin"`
}

func main() {
	coffee := &Plant{Id: 27, Name: "Coffee"}
	coffee.Origin = []string{"Ethiopia", "Brazil"}

	// Преобразуем структуру в XML
	output, err := xml.MarshalIndent(coffee, " ", "  ")
	if err != nil {
		fmt.Printf("Ошибка: %v\n", err)
	}

	os.Stdout.Write([]byte(xml.Header))
	os.Stdout.Write(output)

	// Преобразуем XML в структуру
	data := `
<plant id="27">
  <name>Coffee</name>
  <origin>Ethiopia</origin>
  <origin>Brazil</origin>
</plant>
`
	var p Plant
	if err := xml.Unmarshal([]byte(data), &p); err != nil {
		fmt.Printf("Ошибка: %v", err)
		return
	}

	fmt.Printf("\n\nРезультат разбора: %+v", p)
}
```
Пример вывода:
```xml
<?xml version="1.0" encoding="UTF-8"?>
 <plant id="27">
   <name>Coffee</name>
   <origin>Ethiopia</origin>
   <origin>Brazil</origin>
 </plant>

Результат разбора: {XMLName:{Space: Local:plant} Id:27 Name:Coffee Origin:[Ethiopia Brazil]}
```

## Подробнее
XML появился в конце 90-х, предназначенный для масштабного электронного публицирования, но быстро нашёл применение в вебе. Появились альтернативы, например JSON, которые выделяют за простоту, но возможности валидации документов через схемы и пространства имен в XML остаются мощными средствами для работы со сложными документами. В Go `encoding/xml` справляется с большинством задач, но для огромных документов или обработки потоков рассмотрите использование `xml.NewDecoder` и `xml.NewEncoder` для более низкоуровневого контроля и лучшей производительности.

## Смотрите также
- Пакет `encoding/xml` в Go: https://pkg.go.dev/encoding/xml
- Учебник по XML: https://www.w3schools.com/xml/
- Блог Go об XML: https://blog.golang.org/xml
- Сравнение между JSON и XML: https://www.json.org/xml.html
