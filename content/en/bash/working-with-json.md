---
title:                "Bash recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/working-with-json.md"
---

{{< edit_this_page >}}

## Why Engage in Working with JSON

JSON (JavaScript Object Notation) is a lightweight and popular data interchange format commonly used in web development. It is human-readable and easy to parse, making it a preferred choice for transferring data between client and server. By learning how to work with JSON data, you can enhance your Bash programming skills and build more dynamic and modern applications. 

## How To Work with JSON in Bash

JSON data is built on two structures: objects and arrays. Objects are key-value pairs enclosed in curly braces while arrays are an ordered collection of values enclosed in square brackets. To access data in JSON, we use the `jq` tool, which is a powerful command-line utility for parsing and manipulating JSON data.

Here's an example of a JSON object:

```Bash
$ curl https://jsonplaceholder.typicode.com/posts/1 | jq
```

And the sample output:

```Bash
{
  "userId": 1,
  "id": 1,
  "title": "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
  "body": "quia et suscipit\\nsuscipit recusandae consequuntur expedita et cum\\nreprehenderit molestiae ut ut quas totam\\nnostrum rerum est autem sunt rem eveniet architecto"
}
```

To access a specific value, we can use the `.` operator. For example, to get the post title from the above JSON object, we can use:

```Bash
$ curl https://jsonplaceholder.typicode.com/posts/1 | jq '.title'
```

Which will return:

```Bash
"sunt aut facere repellat provident occaecati excepturi optio reprehenderit"
```

We can also use `jq` to filter and manipulate JSON data. For instance, we can retrieve only specific fields from a JSON array using the `map` function.

```Bash
$ curl https://jsonplaceholder.typicode.com/comments | jq 'map({name: .name, email: .email})'
```

This will return an array of objects with only the `name` and `email` fields.

## Deep Dive into Working with JSON

Besides basic parsing and filtering, `jq` offers a wide range of features that make working with JSON data more efficient. Some of these include selecting multiple keys at once, using regular expressions to filter data, and even performing data transformations. It also supports various output formats, such as CSV, XML, and YAML.

Another useful feature of `jq` is its ability to handle errors and exceptions gracefully. If the input JSON data is malformed, `jq` will still attempt to process it and provide an error message with helpful details about where the issue occurred.

To learn more about `jq` and its capabilities, the official documentation provides in-depth explanations and examples.

## See Also

- Official `jq` documentation: https://stedolan.github.io/jq/manual/
- `jq` tutorial by Codecademy: https://www.codecademy.com/learn/learn-json-parsing
- Bash JSON cheatsheet: https://devhints.io/bash-json
- `jq` GitHub repository: https://github.com/stedolan/jq