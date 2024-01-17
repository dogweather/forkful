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

Working with JSON in Elixir refers to the process of encoding and decoding data in the JSON format. JSON, which stands for JavaScript Object Notation, is a widely used data interchange format for sending and receiving data over the web. Programmers use JSON in Elixir to easily handle and manipulate data in web applications.

## How to:

To encode data in JSON format, use the `Jason.encode!/1` function, passing in the data as an argument.

```Elixir
data = %{name: "John", age: 30}
Jason.encode!(data)
# outputs: "{\"name\":\"John\",\"age\":30}"
```

To decode JSON data into a native Elixir data structure, use the `Jason.decode/1` function.

```Elixir
json_string = "{\"name\":\"Jane\",\"age\":25}"
Jason.decode(json_string)
# outputs: %{"name" => "Jane", "age" => 25}
```

You can also use the `Jason.encode/1` function for a more flexible encoding process that handles invalid data gracefully.

```Elixir
data = %{name: "Sam", age: nil}
Jason.encode(data)
# outputs: "{\"name\":\"Sam\",\"age\":null}"
```

## Deep Dive

JSON was first introduced in 2001 as an alternative to the XML format for data interchange. It is a lightweight and easy to read format that is commonly used in web development due to its compatibility with JavaScript. Other alternatives to JSON include XML, CSV, and YAML, but JSON has become the de facto standard for data exchange on the web.

Under the hood, Elixir uses the Jason library, which is written in C, for encoding and decoding JSON. This provides a fast and efficient way of handling JSON data in Elixir applications.

## See Also

To learn more about working with JSON in Elixir, check out the official Jason library documentation: [https://hexdocs.pm/jason/](https://hexdocs.pm/jason/).

You can also explore alternatives to JSON, such as XML, CSV, and YAML, and determine which format best suits your project's needs.

For a hands-on experience with JSON and Elixir, try building a web application that utilizes JSON for data interchange.