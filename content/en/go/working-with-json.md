---
title:                "Working with json"
html_title:           "Go recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/working-with-json.md"
---

{{< edit_this_page >}}

## Why

JSON, or JavaScript Object Notation, is a popular data format for transmitting and storing data. It is easily readable by both humans and machines, making it a common choice for applications and web services. Working with JSON in Go allows developers to easily retrieve and manipulate data, making it a valuable skill for anyone working with data-intensive projects.

## How To

To work with JSON in Go, we first need to import the `encoding/json` package. This provides functions for encoding and decoding JSON data. 

To encode data into JSON, we can use the `json.Marshal()` function, passing in a Go data structure as the parameter. Let's say we have a struct called `Person` with two fields, `Name` and `Age`. We can encode this into a JSON string using the following code:

```Go
type Person struct {
  Name string
  Age int
}

p := Person{Name: "John", Age: 25}

jsonString, err := json.Marshal(p)
if err != nil {
  fmt.Println("Error encoding JSON:", err)
}
fmt.Println(string(jsonString))
```

The output of this code would be: `{"Name":"John","Age":25}`. We can also specify which fields we want to include in the JSON string by adding tags to the struct fields. For example, `Name string `json:"name"` will result in a JSON key of `"name"` instead of `"Name"`.

To decode JSON into Go data structures, we can use the `json.Unmarshal()` function. This takes a JSON string as the first parameter and a pointer to the Go data structure as the second parameter.

```Go
type Person struct {
  Name string
  Age int
}

var p Person

jsonString := `{"Name":"John","Age":25}`
err := json.Unmarshal([]byte(jsonString), &p)
if err != nil {
  fmt.Println("Error decoding JSON:", err)
}
fmt.Printf("%+v", p)
```

The output of this code would be: `{Name: "John", Age: 25}`. We can also use maps and slices to decode JSON arrays and objects.

## Deep Dive

Go also provides the `json.Decoder` and `json.Encoder` types, which allow for more control and flexibility when working with JSON data. The `Decoder` type is used for reading and decoding JSON data from an `io.Reader` source, such as a file or network connection. The `Encoder` type is used for writing and encoding JSON data to an `io.Writer` destination.

Other features of the `encoding/json` package include the ability to validate JSON data against a custom data structure, and the ability to omit empty struct fields from the encoded JSON string.

## See Also

- [Official Go documentation on working with JSON](https://golang.org/pkg/encoding/json/)
- [A Beginner's Guide to Working with JSON in Go](https://www.sohamkamani.com/blog/2017/10/18/parsing-json-in-golang/)
- [JSON to Go: Convert JSON to Go Structs](https://mholt.github.io/json-to-go/)