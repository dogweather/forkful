---
title:                "Working with TOML"
aliases: - /en/cpp/working-with-toml.md
date:                  2024-01-25T03:39:48.725416-07:00
model:                 gpt-4-1106-preview
simple_title:         "Working with TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/working-with-toml.md"
---

{{< edit_this_page >}}

## What & Why?
TOML (Tom's Obvious, Minimal Language) is a data serialization format easy to read due to its clear semantics. Programmers use TOML for configuration files because it strikes a balance between human readability and machine parsability.

## How to:
To work with TOML in C++, you'll need a library like `toml++`. Here's a quick start:

```C++
#include <toml++/toml.h>
#include <iostream>
#include <fstream>

int main() {
    // Parse TOML from a file
    std::ifstream ifs("config.toml");
    auto config = toml::parse(ifs);

    // Accessing a value
    std::string title = config["title"].value_or("Untitled");
    std::cout << "Title: " << title << '\n';

    // Modify and save TOML
    config["title"] = "New Title";
    std::ofstream ofs("config.toml");
    ofs << config;
}
```

Sample `config.toml`:
```toml
title = "Example"
```

Sample output:
```plaintext
Title: Example
```

## Deep Dive
TOML was created by Tom Preston-Werner in 2013 as an alternative to YAML and JSON. It’s designed to be simple and explicit, mainly for configuration files. Unlike JSON, TOML focuses on being unambiguous, which means it's deterministic in how the document is parsed.

Alternatives to TOML include YAML, which is more permissive in what's allowed, though sometimes at the cost of predictability. JSON, another alternative, is quite strict in structure but not as human-friendly for configurations due to lack of comments and its brace-heavy syntax.

In implementation, `toml++` is a header-only C++17 library that's compliant with the latest TOML specification. It provides a DOM-like interface to navigate and manipulate TOML data, making it straightforward to integrate into projects. The library takes care of the parsing, validation, and output generation, allowing you to get and set TOML data using C++ types.

## See Also
- The TOML GitHub repository: https://github.com/toml-lang/toml
- `toml++`, a C++ library for TOML: https://github.com/marzer/tomlplusplus
- The official TOML documentation with detailed explanations of the format: https://toml.io/en/
