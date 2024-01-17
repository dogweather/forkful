---
title:                "Working with yaml"
html_title:           "Go recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/working-with-yaml.md"
---

{{< edit_this_page >}}

# Go and YAML: A Powerful Combination

## What & Why?
YAML, which stands for "YAML Ain't Markup Language", is a data serialization language that makes it easy to store and transmit structured data. Programmers often use YAML to configure applications, store application data, and communicate between different programming languages. 

## How to:
To work with YAML in Go, we can use the "gopkg.in/yaml.v2" library. Let's look at an example of how we can read and parse a YAML file using this library:

```Go
package main

import (
    "fmt"
    "log"

    "gopkg.in/yaml.v2"
)

type Config struct {
    Database string `yaml:"database"`
    Host     string `yaml:"host"`
    Port     int    `yaml:"port"`
}

func main() {
    // Load YAML file into a struct
    configFile := []byte(`
        database: GoDB
        host: localhost
        port: 3306
    `)
    var config Config
    err := yaml.Unmarshal(configFile, &config)
    if err != nil {
        log.Fatalf("error: %v", err)
    }

    // Print values from the struct
    fmt.Println("Database:", config.Database)
    fmt.Println("Host:", config.Host)
    fmt.Println("Port:", config.Port)
}
```
Output:
```
Database: GoDB
Host: localhost
Port: 3306
```

## Deep Dive:
YAML was first introduced in 2001 by Clark Evans and is often used for configuration files due to its human-readable format. It is more expressive than other data formats like JSON and XML, making it easier for developers to write and understand. YAML also allows for complex data structures, making it a popular choice for storing and transmitting data between different programming languages.

An alternative to YAML is JSON, which is more widely used and supported by most programming languages. However, YAML offers more flexibility and is easier to read and write for humans. It also supports comments, making it more convenient for developers to document their code.

In terms of implementation, the "gopkg.in/yaml.v2" library uses the go-yaml package to parse YAML data. This package is written entirely in Go, making it efficient and fast. It also follows the YAML 1.2 specification, ensuring compatibility with other tools and languages that support YAML.

## See Also:
To learn more about working with YAML in Go, check out the official documentation for the "gopkg.in/yaml.v2" library: https://gopkg.in/yaml.v2. Additionally, you can also explore the go-yaml package and the YAML specification for a deeper understanding of the language.