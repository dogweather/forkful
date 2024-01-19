---
title:                "Working with yaml"
html_title:           "Gleam recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

YAML Ain't Markup Language (YAML) is a human-readable data serialization language. Programmers use it due to its simplicity, readability and compatibility across languages.

## How to:

Gleam currently does not support reading or writing YAML directly, but you can work it out using a suitable library or through the JSON parsing functions in the Gleam stdlib. Assuming that you have a JSON representation of your YAML, you can parse it.

```Gleam
import gleam/json.{Json, DecodeError}
import gleam/map.{Map, empty, update}

fn parse_json(json: String) -> Result(Map(String, Json), DecodeError) {
  json
    |> json.from_string
    |> result.map(fn(j) {
        j
        |> json.to_map
        |> result.map(append_hello)
       })
    |> result.flatten
}

fn append_hello(m: Map(String, Json)) -> Map(String, Json) {
  update(m , "hello", json.string("world"))
}
```

## Deep Dive

YAML finds its roots in languages like HTML, Perl and Python and its development started in 2001. Alternatives to YAML include JSON or XML, although YAML's ability to visibly represent complex data structures can tilt the scales in its favour. When working with YAML in Gleam, due to the lack of native support, one has to perform a workaround such as library implementations, or heavy use of JSON conversion functions in stdlib.

## See Also

Take a deep dive into Gleam with the [Gleam Guide](https://gleam.run/guides/).
Know more about YAML from [YAML Official site](http://yaml.org/spec/1.2/spec.html).
You might find gleam-expect, an assertion library useful during your coding with Gleam [Gleam Expect](https://hexdocs.pm/gleam_expect/readme.html).