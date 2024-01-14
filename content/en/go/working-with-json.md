---
title:                "Go recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/working-with-json.md"
---

{{< edit_this_page >}}

## Why
JSON (JavaScript Object Notation) is a popular data exchange format used in web development and beyond. It offers a simple and lightweight way to store and transmit data, making it a preferred option for many developers. In this blog post, we will explore the basics of working with JSON in Go.

## How To
To work with JSON in Go, we first need to import the "encoding/json" package. This package provides functions for encoding and decoding data into JSON format. Let's take a look at a simple example:

```
package main

import (
  "encoding/json"
  "fmt"
)

type Person struct {
  Name string
  Age int
}

func main() {
  p := Person{Name: "John", Age: 30}
  jsonStr, _ := json.Marshal(p)

  fmt.Println(string(jsonStr))
}

// Output: {"Name": "John", "Age": 30}
```

In the code above, we define a struct called "Person" with two fields: "Name" and "Age". Then, we use the "json.Marshal()" function to encode our "Person" struct into JSON format. Finally, we print out the encoded JSON string.

We can also use the "json.Unmarshal()" function to decode JSON data back into our struct. Let's modify our previous example to include decoding:

```
package main

import (
  "encoding/json"
  "fmt"
)

type Person struct {
  Name string
  Age int
}

func main() {
  p := Person{Name: "John", Age: 30}
  jsonStr, _ := json.Marshal(p)

  var decodedPerson Person
  json.Unmarshal(jsonStr, &decodedPerson)

  fmt.Println(decodedPerson.Name)
  fmt.Println(decodedPerson.Age)
}

// Output: John
// 30
```

Here, we create an empty "Person" struct and use the address operator "&" to pass its reference to the "json.Unmarshal()" function. This function then fills in the struct fields with the corresponding data from the JSON string.

## Deep Dive
When working with JSON in Go, it is important to understand how Go maps data types to JSON types. Here is a quick overview:

- strings, booleans, and all numeric types map directly to their JSON counterparts
- arrays and slices map to JSON arrays
- structs map to JSON objects
- nil maps to JSON null

It is also worth noting that Go will only serialize the exported fields of a struct, i.e. fields that start with a capital letter. If you want to ignore certain fields during encoding, you can use struct tags with the "json" keyword to specify the desired field name.

## See Also
- [JSON and Go: Working with JSON in Go](https://blog.golang.org/json-and-go)
- [Encoding and Decoding JSON with Go](https://www.calhoun.io/encoding-and-decoding-custom-json-with-go/)
- [How to Encode and Decode JSON in Go](https://www.digitalocean.com/community/tutorials/how-to-encode-and-decode-json-data-in-go)

With the examples and information provided in this blog post, you should now have a solid understanding of how to work with JSON in Go. Happy coding!