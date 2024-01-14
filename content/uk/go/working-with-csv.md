---
title:                "Go: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/working-with-csv.md"
---

{{< edit_this_page >}}

## Чому

CSV є одним з найбільш популярних форматів даних для зберігання і обміну інформацією. Використання Go для роботи з CSV дозволяє не тільки ефективно обробляти великі об'єми даних, але й швидко і просто генерувати та зчитувати цей формат. Це робить його необхідним інструментом для багатьох розробників, які працюють з даними.

## Як

Для початку роботи з CSV в Go, спочатку потрібно імпортувати пакет encoding/csv. Далі можна використовувати функції цього пакету для зчитування та запису даних в CSV форматі.

```Go
import "encoding/csv"

// Приклад коду для зчитування даних з CSV файлу
file, err := os.Open("data.csv")
if err != nil {
    log.Fatal(err)
}
defer file.Close()

reader := csv.NewReader(file)
records, err := reader.ReadAll()
if err != nil {
    log.Fatal(err)
}

// Вивід результатів зчитування даних
fmt.Println(records)
```

```Go
import "encoding/csv"

// Приклад коду для запису даних в CSV файл
data := [][]string{
    {"Name", "Age", "City"},
    {"John", "25", "New York"},
    {"Kate", "30", "London"},
}

file, err := os.Create("output.csv")
if err != nil {
    log.Fatal(err)
}
defer file.Close()

writer := csv.NewWriter(file)
writer.WriteAll(data)
writer.Flush()

// Вивід створеного CSV файлу
file, err := os.Open("output.csv")
if err != nil {
    log.Fatal(err)
}
defer file.Close()

reader := csv.NewReader(file)
records, err := reader.ReadAll()
if err != nil {
    log.Fatal(err)
}

fmt.Println(records)
```

## Глибинний аналіз

Пакет encoding/csv дозволяє налаштовувати різні параметри для роботи з CSV файлами, такі як розпізнавання роздільників, ігнорування рядків, налаштування форматування даних тощо. Також, для зберігання додаткової інформації, яка не вміщується в заголовках стовпців, можна використовувати структури типу map.

## Дивіться також

- [Пакет encoding/csv в документації Go](https://golang.org/pkg/encoding/csv/)
- [Стаття "Робота з CSV даними в Go"](https://medium.com/golang-ukraine/%D1%80%D0%BE%D0%B1%D0%BE%D1%82%D0%B0-%D0%B7-csv-%D0%B4%D0%B0%D0%BD%D0%B8%D0%BC%D0%B8-%D0%B2-go-14dbcee12a8f)
- [Відео "Робота з CSV в Go"](https://www.youtube.com/watch?v=gy83pz5kGQ0)