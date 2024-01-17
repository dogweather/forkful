---
title:                "Робота з csv"
html_title:           "Go: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/working-with-csv.md"
---

{{< edit_this_page >}}

Що і чому?

Робота з CSV - це про використання текстових файлів для збереження табличної інформації. Програмісти часто використовують цей формат для обробки даних, так як він легкий для читання та обробки за допомогою програмного забезпечення.

Як:

```go
package main

import (
    "encoding/csv"
    "os"
    "log"
)

func main() {
    // Створюємо CSV файл з даними
    file, err := os.Create("test.csv")
    if err != nil {
        log.Fatal("Error creating file: %s", err)
    }
    defer file.Close()

    // Створюємо писаря CSV
    writer := csv.NewWriter(file)
    // Записуємо рядки в файл CSV
    data := [][]string{
        {"Персона ID", "Ім'я", "Роль"},
        {"001", "Іван", "Аналітик"},
        {"002", "Марія", "Розробник"},
        {"003", "Олег", "Тестувальник"},
    }
    writer.WriteAll(data)
    writer.Flush()

    // Створюємо читача CSV
    reader := csv.NewReader(file)
    // Зчитуємо дані рядками
    for {
        record, err := reader.Read()
        if err != nil {
            break
        }
        fmt.Println(record)
    }
}
```

Виходом буде:

```go
[Персона ID Ім'я Роль]
[001 Іван Аналітик]
[002 Марія Розробник]
[003 Олег Тестувальник]
```

Детальніше:

Стандартний CSV формат був запропонований у 1972 році та використовується для обміну даними в текстовому вигляді. Існують альтернативи, такі як JSON або XML, але CSV залишається популярним для роботи з табличною інформацією. Один з переваг роботи з CSV в тому, що це простий та легкий формат, який можна швидко обробляти за допомогою програмного забезпечення. У Go мові є вбудовані бібліотеки для роботи з CSV, що робить її досить простою для використання.

Дивись також:

- [Стандартна бібліотека CSV у Go](https://golang.org/pkg/encoding/csv/)
- [Стаття про роботу з CSV у Go](https://www.calhoun.io/reading-and-writing-csv-files-in-go/)