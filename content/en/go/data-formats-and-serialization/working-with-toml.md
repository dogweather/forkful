---
date: 2024-02-03 17:50:15.883688-07:00
description: "TOML (Tom's Obvious, Minimal Language) is a configuration file format\
  \ that's easy to read due to its simple syntax. Programmers use TOML to configure\u2026"
lastmod: '2024-02-25T18:49:56.118704-07:00'
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language) is a configuration file format that's\
  \ easy to read due to its simple syntax. Programmers use TOML to configure\u2026"
title: Working with TOML
---

{{< edit_this_page >}}

## What & Why?

TOML (Tom's Obvious, Minimal Language) is a configuration file format that's easy to read due to its simple syntax. Programmers use TOML to configure application settings and dependencies because of its clarity and straightforward mapping to data structures, making it a popular choice in many Go projects for setting up and managing configurations.

## How to:

To start working with TOML in Go, you first need to include a library that can parse TOML files since the Go standard library does not natively support TOML. The `BurntSushi/toml` package is a popular choice for this. First, ensure to install it:

```bash
go get github.com/BurntSushi/toml
```

Here’s a simple example of how to use it. Consider you have a configuration file named `config.toml` with the following content:

```toml
title = "TOML Example"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

Now, you need to create a Go structure that mirrors the TOML structure:

```go
package main

import (
    "fmt"
    "github.com/BurntSushi/toml"
)

type Config struct {
    Title    string
    Database Database `toml:"database"`
}

type Database struct {
    Server        string
    Ports         []int
    ConnectionMax int `toml:"connection_max"`
    Enabled       bool
}

func main() {
    var config Config
    if _, err := toml.DecodeFile("config.toml", &config); err != nil {
        fmt.Println(err)
        return
    }
    fmt.Printf("Title: %s\n", config.Title)
    fmt.Printf("Database Server: %s\n", config.Database.Server)
}
```

Sample output:

```
Title: TOML Example
Database Server: 192.168.1.1
```

## Deep Dive

TOML was created by Tom Preston-Werner, one of the cofounders of GitHub, to offer a straightforward configuration file format that can easily map to a hash table and be understood at a glance without prior knowledge of the format. It contrasts with JSON or YAML, which, while also widely used, can be less human-friendly for configuration files because of braces, quotes, and indentation issues.

The `BurntSushi/toml` package in Go is a robust library that not only allows decoding but also encoding of TOML files, making it a versatile choice for applications that need to both read and write configuration files in this format. However, one should note that with the advancement of technologies and the introduction of newer Go versions, alternatives such as `pelletier/go-toml` have emerged, offering improved performance and additional features like tree manipulation and query support.

While TOML is a great choice for many applications, depending on the complexity of the application configuration and personal or team preferences, other formats like YAML or JSON might be better suited, especially if the configuration requires more complex data structures that TOML's verbose nature might not elegantly capture. Nonetheless, for straightforward, readable, and easily editable configurations, TOML, paired with Go’s strong type system and the aforementioned libraries, is an excellent choice.
