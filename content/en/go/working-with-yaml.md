---
title:                "Go recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why

If you're a Go programmer, you may have come across the term YAML at some point. YAML stands for "YAML Ain't Markup Language" and is a human-readable data serialization format. In simpler terms, it's a way to store and transmit data in a structured and easy to read format. So why would someone want to work with YAML in their Go projects?

YAML has become a popular choice for configuring and managing applications due to its simplicity and readability. It also allows for easy merging and version control of configuration files, making it a great fit for use in software development. Plus, since it's based on key-value pairs and indentation, it's easy for both humans and machines to understand.

## How To

Working with YAML in Go is relatively straightforward. First, you'll need to import the "gopkg.in/yaml.v2" package. This package provides functions for encoding and decoding YAML data.

Let's say we have a YAML file called "config.yaml" with the following content:

```
server:
    port: 8080
    host: localhost
database:
    username: john
    password: pass123
```

To decode this file and access its values in Go, we can use the Unmarshal function from the "gopkg.in/yaml.v2" package. We'll need to create a struct that corresponds to the structure of our YAML file. In our case, it would look like this:

```
type Config struct {
    Server   struct {
        Port int `yaml:"port"`
        Host string `yaml:"host"`
    } `yaml:"server"`
    Database struct {
        Username string `yaml:"username"`
        Password string `yaml:"password"`
    } `yaml:"database"`
}
```

Note the use of the "yaml" tag in each field to specify its corresponding key in the YAML file. We can then use the Unmarshal function to decode the YAML file into our struct and access its values:

```
func main() {
    var config Config
    yamlFile, err := ioutil.ReadFile("config.yaml")
    if err != nil {
        log.Println("Error opening YAML file:", err)
    }
    err = yaml.Unmarshal(yamlFile, &config)
    if err != nil {
        log.Println("Error decoding YAML file:", err)
    }
    fmt.Println("Server Port:", config.Server.Port)
    fmt.Println("Database Username:", config.Database.Username)
}
```

This will output:

```
Server Port: 8080
Database Username: john
```

Of course, this is just one example, and there are many other ways to work with YAML in Go. Check out the "See Also" section below for more resources and examples.

## Deep Dive

For those interested in diving deeper into working with YAML in Go, there are a few things to keep in mind. First, YAML supports various data types, including strings, numbers, booleans, arrays, and maps. Make sure to correctly define the corresponding data types in your Go struct to avoid any decoding errors.

Additionally, you may come across situations where you need to handle comments and tags in your YAML files. The "gopkg.in/yaml.v2" package provides functions for parsing these elements, so be sure to check the documentation for more details.

## See Also

- Official YAML website: https://yaml.org/
- "gopkg.in/yaml.v2" package documentation: https://pkg.go.dev/gopkg.in/yaml.v2
- Example project using YAML in Go: https://github.com/go-yaml/yaml/tree/main/example