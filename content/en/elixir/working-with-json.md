---
title:                "Working with json"
html_title:           "Elixir recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

JSON (JavaScript Object Notation) is a lightweight, text-based data interchange format that's easily human-readable and writeable. It's a linchpin in web development, used for transmitting data in web applications as something easily digestible by the server.

## How to:

Implementing JSON in Elixir is cinch with the Poison library. Suppose we have a simple JSON object.

```Elixir
json_data = ~S({"name": "John Doe", "age": 30})
```

With Poison, we can easily decode this JSON data into a map that Elixir can work with.

```Elixir
{:ok, decoded_data} = Poison.decode(json_data)

IO.inspect(decoded_data)
```

The output would look like:

```Elixir
%{"name" => "John Doe", "age" => 30}
```

Similarly, we can encode Elixir data into JSON like this:

```Elixir
data = %{"name" => "Jane Doe", "age" => 25}
{:ok, encoded_data} = Poison.encode(data)

IO.inspect(encoded_data)
```

The output would be:

```Elixir
"{\"age\":25,\"name\":\"Jane Doe\"}"
```

## Deep Dive

Historically, JSON has trumped XML in terms of popularity due to its simplicity and its seamless integration with JavaScript. It was popularized in the early 2000s as web application development was booming.

There are alternatives to JSON, like XML, YAML, or even plain text, but each comes with its own pros and cons. XML is verbose and complex, YAML is human-friendly but less common, and plain text doesn't offer the structure necessary for complex data.

When it comes to decoding and encoding JSON in Elixir, libraries like Poison work by parsing the JSON and converting it into elixir-native map structures. Taking advantage of Elixir's pattern matching, they also provide methods for encoding Elixir data structures back into JSON.

## See Also

- Poison Library: https://github.com/devinus/poison
- JSON Specifications: https://www.json.org/json-en.html
- Elixir Docs: https://hexdocs.pm/elixir/Kernel.html
- Alternatives to JSON: https://www.drdobbs.com/web-development/alternatives-to-json-are-they-any-goo/240168719