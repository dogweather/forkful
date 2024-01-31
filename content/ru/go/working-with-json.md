---
title:                "Работа с JSON"
date:                  2024-01-29T00:04:29.586120-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с JSON"

category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/go/working-with-json.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Работа с JSON заключается в кодировании и декодировании данных в формате JavaScript Object Notation, текстовом способе представления структурированных данных. Программисты используют его из-за его простоты и повсеместности в веб-API и файлах конфигурации.

## Как:

### Маршалинг JSON в Go

```Go
package main

import (
	"encoding/json"
	"fmt"
)

type User struct {
	Name   string `json:"name"`
	Age    int    `json:"age"`
	Active bool   `json:"active"`
}

func main() {
	user := User{Name: "Алиса", Age: 25, Active: true}
	jsonData, err := json.Marshal(user)
	if err != nil {
		panic(err)
	}
	fmt.Println(string(jsonData))
}
```

Пример вывода:
```json
{"name":"Алиса","age":25,"active":true}
```

### Демаршалинг JSON в Go

```Go
package main

import (
	"encoding/json"
	"fmt"
)

func main() {
	var jsonData = []byte(`{"name":"Алиса","age":25,"active":true}`)
	user := User{}
	err := json.Unmarshal(jsonData, &user)
	if err != nil {
		panic(err)
	}
	fmt.Printf("%+v\n", user)
}

type User struct {
	Name   string `json:"name"`
	Age    int    `json:"age"`
	Active bool   `json:"active"`
}
```

Пример вывода:
```
{Name:Алиса Age:25 Active:true}
```

## Глубокое погружение

JSON, возникший из JavaScript, стал стандартом для обмена данными в середине 2000-х. По сравнению с XML он более легковесный и читаемый для человека, поэтому является предпочтительным для RESTful API. В Go пакет `encoding/json` обрабатывает данные JSON, используя теги полей структур для соответствия ключей JSON с полями структур.

Альтернативы JSON включают XML, YAML и бинарные форматы, такие как Protocol Buffers (protobuf). Каждый имеет свои случаи использования; например, YAML предпочтителен для файлов конфигурации, написанных человеком, тогда как protobuf используется для эффективной нейтральной к платформе передачи сериализованных данных.

Go реализует обработку JSON эффективно, хотя использование отражения может сделать его медленнее по сравнению с некоторыми механизмами сериализации, которые могут работать во время компиляции.

## Смотрите также

- Блог Go о JSON: https://blog.golang.org/json
- Документация пакета Go `encoding/json`: https://pkg.go.dev/encoding/json
- Официальный сайт стандарта JSON: http://json.org/
