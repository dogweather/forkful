---
title:                "Working with TOML"
date:                  2024-01-25T03:39:50.811207-07:00
model:                 gpt-4-1106-preview
simple_title:         "Working with TOML"

category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/working-with-toml.md"
---

{{< edit_this_page >}}

## What & Why?
Working with TOML involves parsing and encoding TOML (Tom's Obvious, Minimal Language) files in Go. Programmers opt for TOML for its readability and easy mapping to data structures, a solid fit for configs.

## How to:
To work with TOML in Go, you'll typically use a library like `BurntSushi/toml`. Here's a quick look at parsing a TOML config file:

```Go
package main

import (
    "fmt"
    "os"

    "github.com/BurntSushi/toml"
)

type Config struct {
    Title   string
    Owner   struct {
        Name string
    }
}

func main() {
    var config Config
    if _, err := toml.DecodeFile("config.toml", &config); err != nil {
        fmt.Println(err)
        return
    }
    fmt.Printf("Title: %s, Owner: %s\n", config.Title, config.Owner.Name)
}
```

Sample `config.toml`:

```Toml
title = "Example TOML"
[owner]
name = "Tom Preston-Werner"
```

Sample output:

```
Title: Example TOML, Owner: Tom Preston-Werner
```

## Deep Dive
TOML, introduced by Tom Preston-Werner in 2013, was designed to be a minimal configuration file format that's easy to read due to its clear semantics. Go developers often use TOML for configuration over alternatives like JSON or YAML for its straightforwardness and ability to represent complex hierarchies with simplicity.

Compared to YAML, which has complex features and potential security concerns, TOML's flat design reduces complexity and typo-induced errors. And unlike JSON, TOML supports comments, making it easier to explain configurations in-line.

When working with TOML in Go, there are nuances to consider. Struct tags can customize how your structs map to TOML structures, and you should also be aware of how TOML arrays and inline tables are parsed into Go slices and maps.

## See Also
- TOML Specification: https://toml.io/en/
- BurntSushi/toml Library: https://github.com/BurntSushi/toml
- A comparison of config file formats: https://www.redhat.com/sysadmin/yaml-toml-json-differences
