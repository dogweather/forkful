---
title:                "Working with json"
html_title:           "Gleam recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/working-with-json.md"
---

{{< edit_this_page >}}

# Working with JSON in Gleam Programming 

## What & Why?

JavaScript Object Notation, JSON, often plays key roles in server-to-server communication and storage of complex data. Programmers use it as a language-independent format to pass data across network connections.

## How to:

We'll learn to encode and decode JSON in Gleam. For decoding, we'll define our type to map JSON and then "decode" it. 

```Gleam
import gleam/decode.{Decoder, int, map2, field}
import gleam_codecs.json

type Person {
  Person(name: String, age: Int)
}

// Our decoder
fn decode_person(json: String) -> Result(Person, String) {
  let decoder: Decoder(Person) =
    map2(name: field("name", string), age: field("age", int), Person)

  json_codecs.decode(json, decoder)
}
```

This 'decode_person' function takes a JSON string and returns the 'Person'.

Encoding is straightforward, just serialize it using 'encode'

```Gleam
import gleam_codecs.json

fn encode_person(person: Person) -> String {
  json_codecs.encode(person)
}
```

The output would be:

```
{ "name": "Gleam", "age": 2 }
```

## Deep Dive

JSON, derived from JavaScript but language-agnostic, became popular in late 2000s as a payload format for web APIs and config files. It's human-readable and easy to parse, thus a great choice for data interchange.

Alternatives include XML and YAML, though JSON's less verbosity gives it an edge. CSV is used mostly for simpler data.

For JSON in Gleam, we use the 'gleam/decode' lib. Decoding is explicit due to statically typed nature of Gleam. We first define our type, then tell Gleam how to decode it. Encoding, on the other hand, is pretty straightforward.

## See Also

For detailed JSON and Gleam understanding, look at:
1. Gleam JSON Decoding [Docs](https://hexdocs.pm/gleam_decode/readme.html)
2. Gleam's Github [Repo](https://github.com/gleam-lang/gleam)
3. JSON Documentation on [MDN](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/JSON)