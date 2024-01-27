---
title:                "Working with JSON"
date:                  2024-01-19
html_title:           "Arduino recipe: Working with JSON"
simple_title:         "Working with JSON"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

Working with JSON means encoding and decoding data in the JavaScript Object Notation format, a text-based way to represent structured data. Programmers use it for its simplicity and ubiquity in web APIs and config files.

## How to:

### Marshalling JSON in Go

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
	user := User{Name: "Alice", Age: 25, Active: true}
	jsonData, err := json.Marshal(user)
	if err != nil {
		panic(err)
	}
	fmt.Println(string(jsonData))
}
```

Sample output:
```json
{"name":"Alice","age":25,"active":true}
```

### Unmarshalling JSON in Go

```Go
package main

import (
	"encoding/json"
	"fmt"
)

func main() {
	var jsonData = []byte(`{"name":"Alice","age":25,"active":true}`)
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

Sample output:
```
{Name:Alice Age:25 Active:true}
```

## Deep Dive

JSON, birthed from JavaScript, became a standard for data interchange in the mid-2000s. Compared to XML, it’s lighter and human-readable, which is why it's the go-to for RESTful APIs. In Go, the `encoding/json` package handles JSON data, using structs' field tags to match JSON keys with struct fields.

Alternatives to JSON include XML, YAML, and binary formats like Protocol Buffers (protobuf). Each has its use cases; for instance, YAML is preferred for human-written config files, while protobuf is used for efficient, platform-neutral serialized data transfer.

Go implements JSON handling efficiently, though using reflection can cause it to be slower in comparison to some serialization mechanisms that can work at compile-time.

## See Also

- The Go Blog on JSON: https://blog.golang.org/json
- Go `encoding/json` package docs: https://pkg.go.dev/encoding/json
- JSON official site for the standard: http://json.org/
