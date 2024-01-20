---
title:                "Working with yaml"
html_title:           "Arduino recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML, "YAML Ain't Markup Language", is a human-friendly data serialization standard. Programmers use it for config files, data exchange between languages, and because it's more readable than JSON or XML.

## How to:
Gleam currently doesn't have built-in YAML parsers or libraries, as of my last update. You’d typically parse YAML in Gleam by leaning on Erlang functions thanks to Gleam's compatibility with Erlang’s ecosystem. Let’s use an Erlang library and call it from Gleam.

First, add the Erlang YAML library to `rebar.config`:

```erlang
{deps, [yaml]}.
```

Here's how you can call the Erlang library from Gleam:

```rust
external fn parse(String) -> Result(Tuple(tuple(atom(), String)), Nil) =
  "yaml" "decode"

pub fn main() -> Result(Tuple(tuple(atom(), String)), Nil) {
  let yaml_data = "greeting: hello"
  parse(yaml_data)
}
```

Sample output might look like this:

```elixir
Ok(#(ok, [{greeting, "hello"}]))
```

## Deep Dive
YAML was released in 2001, and it’s often used where human readability is important. It's not always the default for data serialization, JSON and XML are widely used too. However, YAML's simplicity makes it ideal for config files or simple data structures.

Alternatives might be Elixir’s built-in `:yamerl` parser, and in Gleam, you might handle similar tasks using JSON with the `gleam/json` library. As for implementation, you're tapping into the broader BEAM ecosystem when you work with YAML in Gleam—it's this interoperability that makes parsing YAML possible without needing a dedicated Gleam library.

## See Also
- YAML specification: https://yaml.org/spec/1.2/spec.html
- Erlang `yaml` library: https://hex.pm/packages/yaml
- Gleam’s JSON library documentation: https://hexdocs.pm/gleam_stdlib/gleam/json/