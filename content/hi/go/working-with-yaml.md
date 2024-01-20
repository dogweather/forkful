---
title:                "यामल के साथ काम करना"
html_title:           "C#: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
YAML एक data serialization format है जिसका इस्तेमाल configuration files और data exchange में होता है। Programmers इसका इस्तेमाल इसलिए करते हैं क्योंकि यह पढ़ने में आसान है और diverse data structures को support करता है।

## How to: (कैसे करें:)
Go में YAML को handle करने के लिए, `go-yaml` library का प्रयोग करें। यहाँ एक basic example है:

```Go
package main

import (
	"fmt"
	"gopkg.in/yaml.v2"
	"log"
)

// Config struct represents the structure of our YAML configuration
type Config struct {
	Title       string
	Description string
	Database    struct {
		User     string
		Password string
	}
}

func main() {
	data := `
title: My Project
description: A sample project with YAML config
database:
  user: admin
  password: secret
`

	var config Config
	err := yaml.Unmarshal([]byte(data), &config)
	if err != nil {
		log.Fatalf("error: %v", err)
	}
	fmt.Printf("Title: %s\nDescription: %s\nDatabase User: %s\n", config.Title, config.Description, config.Database.User)
}
```
Sample Output:
```
Title: My Project
Description: A sample project with YAML config
Database User: admin
```

## Deep Dive (गहराई से जानकारी:)
YAML (YAML Ain't Markup Language) शुरू में XML का एक सरल रूप प्रदान करने के लिए 2001 में develop किया गया था। इसके विकल्पों में JSON और XML शामिल हैं। YAML complex data dependencies को आसानी से संभाल सकता है और Go में इसका implementation `go-yaml` library के माध्यम से किया जाता है जो reflection का उपयोग कर object properties को map करता है।

## See Also (और भी जानकारी पाएं:)
- YAML official specification: [YAML Specification](https://yaml.org/spec/)
- go-yaml library documentation: [go-yaml](https://pkg.go.dev/gopkg.in/yaml.v2)
- JSON in Go: [JSON in Go](https://pkg.go.dev/encoding/json)