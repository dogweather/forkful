---
title:                "Working with TOML"
date:                  2024-01-25T03:40:03.098754-07:00
model:                 gpt-4-1106-preview
simple_title:         "Working with TOML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/working-with-toml.md"
---

{{< edit_this_page >}}

## What & Why?
Working with TOML means parsing and generating TOML (Tom's Obvious, Minimal Language) files with code. Programmers use TOML for easy-to-read config files and data serialization, thanks to its clear semantics and compatibility with conventional data types.

## How to:
Gleam doesn't have built-in TOML support, so you'll need an external library. For example:

```gleam
// Assuming you have a TOML parsing library:
import toml/{Parser, Encoder}

// Parse TOML content
let toml_content = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

let parsed = Parser.parse(toml_content)

// Use the parsed data
match parsed {
  Ok(data) -> "Data parsed successfully!"
  Error(_) -> "Failed to parse data."
}

// Generate TOML content from Gleam data structure
let data = #{
  "owner": #{
    "name": "Tom Preston-Werner",
    "dob": "1979-05-27T07:32:00Z"
  }
}

let toml_string = Encoder.encode(data)
```

Sample output:

```
Data parsed successfully!
```

## Deep Dive
TOML was released in 2013 by Tom Preston-Werner. Its goal: to be more readable and straightforward than XML and less complex than YAML for file configurations. Despite the simplicity, it's robust for structured data, offering explicit and easy-to-understand syntax. Alternatives include JSON, YAML, and INI, but TOML's minimalistic and clear syntax often wins out for config files. Implementing TOML in Gleam involves two main actions: parsing TOML into native data structures and serializing native data structures into TOML. Most TOML libraries for Erlang or Elixir can be used in Gleam due to its interoperability with BEAM languages, ensuring seamless integration within Gleam projects.

## See Also
- TOML language specs: [https://toml.io/en/](https://toml.io/en/)
- An Erlang TOML parser: [https://hex.pm/packages/toml](https://hex.pm/packages/toml)
- TOML on GitHub: [https://github.com/toml-lang/toml](https://github.com/toml-lang/toml)
