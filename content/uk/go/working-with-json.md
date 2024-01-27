---
title:                "Робота з JSON"
date:                  2024-01-19
html_title:           "Arduino: Робота з JSON"
simple_title:         "Робота з JSON"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON - це популярний формат обміну даними. Використовуємо його в Go для спрощення серіалізації та десеріалізації структур даних, особливо при спілкуванні з веб-сервісами.

## How to:
```Go
package main

import (
	"encoding/json"
	"fmt"
	"log"
)

// Структура для прикладу
type User struct {
	Name   string `json:"name"`
	Age    int    `json:"age"`
	Active bool   `json:"active"`
}

func main() {
	// Серіалізація даних у JSON
	user := User{"Олекса", 25, true}
	userJSON, err := json.Marshal(user)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(string(userJSON))

	// Десеріалізація JSON у дані
	jsonStr := `{"name":"Марія","age":30,"active":false}`
	var user2 User
	err = json.Unmarshal([]byte(jsonStr), &user2)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("%+v\n", user2)
}
```
**Вивід:**
```
{"name":"Олекса","age":25,"active":true}
{Name:Марія Age:30 Active:false}
```

## Deep Dive
JSON запровадили на початку 2000-х як більш просту альтернативу XML. JSON та YAML – популярні альтернативи, але JSON легше інтегрувати через ширшу підтримку та ефективність у веб-застосунках. Go має вбудований пакет `encoding/json` для роботи з JSON, що використовує рефлексію для надання гнучкості при серіалізації та десеріалізації.

## See Also
- Документація Go by Example про JSON: [https://gobyexample.com/json](https://gobyexample.com/json)
- Специфікація JSON: [https://www.json.org/json-uk.html](https://www.json.org/json-uk.html)
- Лекція про пакет `encoding/json` Go: [https://blog.golang.org/json](https://blog.golang.org/json)
