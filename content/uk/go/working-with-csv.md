---
title:                "Робота з CSV файлами"
date:                  2024-01-19
html_title:           "Arduino: Робота з CSV файлами"
simple_title:         "Робота з CSV файлами"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Що таке обробка CSV і навіщо це програмістам? CSV, або "Comma-Separated Values", це формат файлу, що зберігає таблицеві дані. Програмісти використовують CSV для легкого обміну даними між різними системами.

## How to:
```Go
package main

import (
	"encoding/csv"
	"fmt"
	"os"
	"strings"
)

func main() {
	// Читання CSV зі строки.
	csvData := "назва,ціна,кількість\nручка,10.99,100\nолівець,5.49,300"
	reader := csv.NewReader(strings.NewReader(csvData))
	records, _ := reader.ReadAll()

	// Вивід прочитаних даних.
	for _, record := range records {
		fmt.Println("Товар:", record[0])
		fmt.Println("Ціна:", record[1])
		fmt.Println("Кількість:", record[2])
	}

	// Запис CSV у файл.
	file, _ := os.Create("products.csv")
	writer := csv.NewWriter(file)
	writer.WriteAll(records) // Зазвичай треба перевіряти помилки.
	writer.Flush()
	file.Close()
}
```
Приклад виводу:
```
Товар: назва
Ціна: ціна
Кількість: кількість
Товар: ручка
Ціна: 10.99
Кількість: 100
Товар: олівець
Ціна: 5.49
Кількість: 300
```

## Deep Dive
CSV з'явився ще у 1970-х. Через сумісність та простоту продовжує бути популярним. Альтернативи – це JSON, XML, але CSV швидший для читання і письма людиною. У Go для CSV є стандартний пакет `encoding/csv`, який надає інструментарій для ефективної роботи з цим форматом.

## See Also
- [Офіційна документація по пакету encoding/csv](https://pkg.go.dev/encoding/csv)
- [Стаття "Робота з CSV на Go"](https://www.golangprograms.com/golang-read-csv-file-into-struct.html)
- [Використання Go для роботи з різними форматами файлів](https://medium.com/@ankurraina/using-go-for-file-handling-3e4cf0753494)
