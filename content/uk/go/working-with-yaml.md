---
title:                "Робота з yaml"
html_title:           "Go: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## Що і Чому?

Робота з YAML - це процес роботи зі структурованим форматом даних, який часто використовується при програмуванні. Програмісти використовують YAML для збереження та передачі даних у зручному для обробки форматі.

## Як:

```Go
package main 

import "fmt"
import "gopkg.in/yaml.v2"

type User struct {
    Name  string `yaml:"name"`
    Email string `yaml:"email"`
}

func main() {
    user := User{Name: "John Doe", Email: "johndoe@example.com"}

    // Encoding
    data, err := yaml.Marshal(user)
    if err != nil {
        panic(err)
    }
    fmt.Printf(string(data))

    // Decoding
    var decodedUser User
    err = yaml.Unmarshal([]byte(data), &decodedUser)
    if err != nil {
        panic(err)
    }
    fmt.Println(decodedUser.Name, decodedUser.Email)
}
```

Вихідний код:
```text
name: John Doe
email: johndoe@example.com
```
Вивід:
```
John Doe johndoe@example.com
```

## Глибинний Заплив:

YAML був створений у 2001 році з метою створення простого та зручного для читання формату даних, який був би підходящим для використання у скриптах та інструментах. Існують альтернативи, такі як JSON та XML, але деякі програмісти віддають перевагу YAML через його зручну інтуїтивно зрозумілу структуру. У Go існує бібліотека gopkg.in/yaml.v2, яка підтримує кодування та декодування даних в форматі YAML.

## Дивіться також:

- [Офіційна документація YAML](https://yaml.org/)
- [Документація для бібліотеки gopkg.in/yaml.v2](https://pkg.go.dev/gopkg.in/yaml.v2)