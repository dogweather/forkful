---
title:                "Go: Робота з json"
simple_title:         "Робота з json"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/working-with-json.md"
---

{{< edit_this_page >}}

## Чому

JSON, або JavaScript Object Notation, є одним з найпопулярніших форматів обміну даними у світі програмування. Робота з JSON є важливою навичкою для розробників, оскільки цей формат використовується у багатьох сучасних додатках і веб-сайтах. Крім того, використання Go для роботи з JSON дозволяє зробити процес ще більш ефективним та швидким.

## Як

Для початку, необхідно імпортувати пакет encoding/json у свій проект Go. Далі, використовуючи функцію `json.Marshal ()`, можна перетворити дані у формат JSON. Нижче наведений приклад коду, який демонструє цей процес:

```Go
type User struct {
    Name     string `json:"name"`
    Age      int    `json:"age"`
    Location string `json:"location"`
}

user := User{
    Name:     "Іван",
    Age:      25,
    Location: "Київ",
}

jsonData, err := json.Marshal(user)

if err != nil {
    fmt.Println("Помилка при перетворенні у формат JSON:", err)
} else {
    fmt.Println(string(jsonData))
}

// Результат: {"name": "Іван", "age": 25, "location": "Київ"}
```

Крім цього, використовуючи функцію `json.Unmarshal ()`, можна перетворити дані у форматі JSON назад у звичайну структуру даних. Нижче наведено приклад коду, який це демонструє:

```Go
type Car struct {
    Brand  string `json:"brand"`
    Model  string `json:"model"`
    Year   int    `json:"year"`
}

carJSON := `{"brand":"Audi", "model":"A6", "year":2018}`

var car Car

err := json.Unmarshal([]byte(carJSON), &car)

if err != nil {
    fmt.Println("Помилка при перетворенні з формату JSON:", err)
} else {
    fmt.Println("Марка:", car.Brand)
    fmt.Println("Модель:", car.Model)
    fmt.Println("Рік:", car.Year)
}

// Результат:
// Марка: Audi
// Модель: A6
// Рік: 2018
```

## Глибока здивовані

Хоча робота з JSON може здатися простим процесом, вона може бути досить складною, особливо коли мова йде про більш складні дані. Наприклад, виправляння помилок під час парсингу може бути важким завданням. Також, важливо розуміти структуру та будову цього формату.

Щоб дізнатися більше про роботу з JSON у Go, варто детальніше ознайомитися з офіційною документацією та навчальними ресурсами.

## Дивись також

- [Офіційна документація по роботі з JSON в Go](https://golang.org/pkg/encoding/json/)
- [Відеоуроки по роботі з JSON у Go](https://www.youtube.com/watch?v=8SWXQf0r1EY)
- [Стаття про