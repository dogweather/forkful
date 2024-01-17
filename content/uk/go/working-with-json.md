---
title:                "Робота з json"
html_title:           "Go: Робота з json"
simple_title:         "Робота з json"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/working-with-json.md"
---

{{< edit_this_page >}}

## Що і чому?

Робота з JSON - це процес обміну даними між різними програмами та системами з використанням формату JSON. Це дуже популярний спосіб передачі даних, оскільки JSON є простим у використанні та зрозумінні форматом. Програмісти використовують JSON для обміну даними з різними сервісами та веб-додатками.

## Як це зробити:

```Go
type User struct {
  Name string `json:"name"`
  Age  int    `json:"age"`
}

// Створення JSON об'єкта
func main() {
  user := User{Name: "John Doe", Age: 24}
  jsonString, _ := json.Marshal(user)
  fmt.Println(string(jsonString)) // {"name":"John Doe","age":24}
}

// Отримання даних з JSON об'єкта
func main() {
  jsonString := `{"name":"Jane Doe","age":28}`
  var user User
  json.Unmarshal([]byte(jsonString), &user)
  fmt.Println(user.Name, user.Age) // Jane Doe 28
}
```

## Глибоке занурення:

JSON був створений як спосіб поєднати дві популярні технології - JavaScript та обмін даними. Це обрало велику популярність завдяки своїй простоті та універсальності. Існують також альтернативні формати, такі як XML та YAML, але JSON залишається простішим та ширше використовується. У програмуванні з Go, робота з JSON є необхідною для забезпечення взаємодії з різними сервісами та додатками.

## Дивись також:

- [Офіційна документація Go для роботи з JSON] (https://golang.org/pkg/encoding/json/)
- [Стаття "Розуміння JSON: Що це таке та як з ним працювати" (англ.)] (https://blog.golang.org/json-and-go)
- [Стаття "Робота з JSON в Go" (рос.)] (https://habr.com/en/post/317266/)