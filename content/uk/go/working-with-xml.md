---
title:                "Робота з XML"
date:                  2024-01-26T04:31:57.851335-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/working-with-xml.md"
---

{{< edit_this_page >}}

## Що і чому?
Робота з XML включає аналіз, створення та маніпулювання XML-документами за допомогою коду. Програмісти роблять це для обміну даними, файлів конфігурації та веб-сервісів, оскільки читабельність XML і його широка підтримка роблять його надійним вибором для структурованих даних.

## Як:
У Go скористайтеся пакетом `encoding/xml`. Давайте аналізувати та генерувати XML.
```go
package main

import (
	"encoding/xml"
	"fmt"
	"os"
)

// Структури відображаються на елементи XML
type Plant struct {
	XMLName xml.Name `xml:"plant"`
	Id      int      `xml:"id,attr"`
	Name    string   `xml:"name"`
	Origin  []string `xml:"origin"`
}

func main() {
	coffee := &Plant{Id: 27, Name: "Coffee"}
	coffee.Origin = []string{"Ethiopia", "Brazil"}

	// Маршалювання структури в XML
	output, err := xml.MarshalIndent(coffee, " ", "  ")
	if err != nil {
		fmt.Printf("Error: %v\n", err)
	}

	os.Stdout.Write([]byte(xml.Header))
	os.Stdout.Write(output)

	// Розмаршалювання XML в структуру
	data := `
<plant id="27">
  <name>Coffee</name>
  <origin>Ethiopia</origin>
  <origin>Brazil</origin>
</plant>
`
	var p Plant
	if err := xml.Unmarshal([]byte(data), &p); err != nil {
		fmt.Printf("Error: %v", err)
		return
	}

	fmt.Printf("\n\nРозмаршалювано: %+v", p)
}
```
Приклад виводу:
```xml
<?xml version="1.0" encoding="UTF-8"?>
 <plant id="27">
   <name>Coffee</name>
   <origin>Ethiopia</origin>
   <origin>Brazil</origin>
 </plant>

Розмаршалювано: {XMLName:{Space: Local:plant} Id:27 Name:Coffee Origin:[Ethiopia Brazil]}
```

## Поглиблене дослідження
XML існує з кінця 90-х років, розроблений для масштабних електронних публікацій, але швидко був впроваджений для вебу. Альтернативи, як-от JSON, з'явилися, рекламувані за простоту, але валідація документів XML через схеми та простори імен залишається потужною для складних документів. У Go `encoding/xml` виконує більшість завдань, але для великих документів або обробки потоків слід розглянути `xml.NewDecoder` і `xml.NewEncoder` для контролю на нижчому рівні та кращої продуктивності.

## Дивіться також
- Пакет Go `encoding/xml`: https://pkg.go.dev/encoding/xml
- XML-підручник: https://www.w3schools.com/xml/
- Блог Go про XML: https://blog.golang.org/xml
- Порівняння між JSON та XML: https://www.json.org/xml.html
