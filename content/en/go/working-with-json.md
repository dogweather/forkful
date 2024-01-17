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

## What & Why?

JSON, short for JavaScript Object Notation, is a popular format for storing and transmitting data in a human-readable way. It is commonly used in web development and data transfer as an alternative to heavy and complex formats like XML. Programmers use JSON to organize and exchange data between different systems and languages, making it an essential tool for efficient and seamless communication.

## How to:

To work with JSON in a Go program, the built-in "encoding/json" package provides helpful functions to easily marshal (convert to JSON) and unmarshal (convert from JSON) data. Here's an example of how to marshal a map into JSON:

```
Go mapToJson 
import (
	"encoding/json"
	"fmt"
)

func main() {
    //Create a map with sample data
    data := map[string]string{
        "name":  "John",
        "age":   "25",
        "title": "Software Engineer",
    }
    
    //Marshal the data into JSON
    jsonData, err := json.Marshal(data)
    if err != nil {
        fmt.Println(err)
        return
    }
    
    //Print the JSON data
    fmt.Println(string(jsonData))
}
```
Output: ```{"age":"25","name":"John","title":"Software Engineer"}```

On the other hand, if you have JSON data and want to unmarshal it into a Go struct, you can do it as follows:
```
Go jsonToStruct 
import (
	"encoding/json"
	"fmt"
)

func main() {
    //Create a JSON string with sample data 
    jsonString := `{"name":"John", "age":"25", "title":"Software Engineer"}`
    
    //Unmarshal the data into a struct
    type Person struct {
        Name  string `json:"name"`
        Age   string `json:"age"`
        Title string `json:"title"`
    }
    var person Person
    err := json.Unmarshal([]byte(jsonString), &person)
    if err != nil {
        fmt.Println(err)
        return
    }
    
    //Print the retrieved data from the struct
    fmt.Println("Name: ", person.Name)
    fmt.Println("Age: ", person.Age)
    fmt.Println("Title: ", person.Title)
}
```
Output: 
```
Name: John
Age: 25
Title: Software Engineer
```

## Deep Dive:

JSON was first introduced in 2001 by Douglas Crockford as a lightweight alternative to XML. It is based on a subset of the JavaScript language, making it easy for web developers to use. Other alternatives to JSON include YAML, BSON, and CSV, each having its own pros and cons. However, it is widely accepted that JSON is the best choice for web-related data exchange due to its simplicity, compatibility, and popularity.

Behind the scenes, the encoding/json package in Go uses the "reflect" package to analyze and convert data structures. This allows it to handle complex types such as maps, structs, and arrays. Additionally, you can use tags on struct fields to control the generated JSON keys. Moreover, the package provides options for pretty-printing JSON and handling null values.

## See Also:

- [Official Go documentation on JSON](https://golang.org/pkg/encoding/json/)
- [JSON syntax rules](https://www.json.org/json-en.html)
- [Difference between JSON and XML](https://www.thoughtco.com/the-differences-between-json-and-xml-2033970)