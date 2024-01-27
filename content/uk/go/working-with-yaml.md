---
title:                "Робота з YAML"
date:                  2024-01-19
html_title:           "Arduino: Робота з YAML"
simple_title:         "Робота з YAML"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
YAML - це формат зберігання даних, легкий для читання людиною. Програмісти використовують для конфігурації додатків, серіалізації даних.

## How to: (Як це зробити:)
```Go
package main

import (
	"fmt"
	"gopkg.in/yaml.v2"
)

// Структура для даних
type Config struct {
	Host string `yaml:"host"`
	Port int    `yaml:"port"`
}

func main() {
	// YAML приклад
	data := `
host: "localhost"
port: 8080
`
	// Ініціалізація структури Config
	var config Config

	// Вилучення YAML в структуру
	err := yaml.Unmarshal([]byte(data), &config)
	if err != nil {
		panic(err)
	}

	// Виведення результату
	fmt.Printf("Host: %s\nPort: %d\n", config.Host, config.Port)
}
```
Sample output:
```
Host: localhost
Port: 8080
```

## Deep Dive (Поглиблений огляд)
YAML (YAML Ain't Markup Language) з'явився у 2001 році. Це альтернатива XML і JSON, простіший для людського сприйняття. Працюючи з YAML у Go потребує зовнішньої бібліотеки, наприклад `gopkg.in/yaml.v2`.

## See Also (Дивіться також)
- YAML офіційний сайт: https://yaml.org
- Go документація на gopkg.in/yaml.v2: https://pkg.go.dev/gopkg.in/yaml.v2
- YAML Wikipedia стаття: https://uk.wikipedia.org/wiki/YAML
- JSON та XML для порівняння: https://www.json.org/json-uk.html, https://www.w3.org/XML/
