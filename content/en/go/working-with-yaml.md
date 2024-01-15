---
title:                "Working with yaml"
html_title:           "Go recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why

Working with YAML can greatly simplify and streamline the process of configuring and managing applications and services. With its easy-to-read syntax and support for structured data, YAML is a popular choice for developers and system administrators alike.

## How To

To work with YAML in Go, we will be using the `gopkg.in/yaml.v2` package. First, we will need to import the package in our code:

```Go
import "gopkg.in/yaml.v2"
```

### Reading YAML

To read a YAML file, we can use the `yaml.Unmarshal()` function, passing in the YAML data as a slice of bytes and a pointer to a structure that will hold the data:

```Go
type Config struct {
    DBHost string `yaml:"db_host"`
    DBPort int `yaml:"db_port"`
    DBUser string `yaml:"db_user"`
    DBPass string `yaml:"db_pass"`
}

// Read YAML file into config struct
var config Config
file, err := os.Open("config.yaml")
if err != nil {
    panic(err)
}
defer file.Close()

yamlData, _ := ioutil.ReadAll(file)

err = yaml.Unmarshal(yamlData, &config)
if err != nil {
    panic(err)
}

fmt.Println(config.DBHost)
fmt.Println(config.DBPort)
```

Note that we have defined tags above each field in our struct (e.g. `yaml:"db_host"`) to specify how the fields should be mapped to the YAML data. This allows our code to easily parse the YAML and assign the values to the corresponding fields.

### Writing YAML

To write YAML data, we can use the `yaml.Marshal()` function, passing in a struct or map that contains the data we want to write:

```Go
type User struct {
    Name string `yaml:"name"`
    Age int `yaml:"age"`
    Email string `yaml:"email"`
}

// Write YAML data
user := User {
    Name: "John Smith",
    Age: 30,
    Email: "john@example.com",
}

yamlData, err := yaml.Marshal(user)
if err != nil {
    panic(err)
}

fmt.Println(string(yamlData))
```

This will output the following YAML:

```Go
name: John Smith
age: 30
email: john@example.com
```

We can also write YAML to a file using the `ioutil.WriteFile()` function to create a new YAML file or overwrite an existing one:

```Go
err = ioutil.WriteFile("new.yaml", yamlData, 0644)
if err != nil {
    panic(err)
}
```

## Deep Dive

YAML supports a wide range of data types, including strings, integers, booleans, arrays, and maps. It also supports the use of comments, making it easier to document and explain the settings and configurations in a file.

One of the main advantages of YAML is its human-readable and intuitive syntax. This makes it easier for developers and system administrators to understand and modify configuration files without needing to learn a complex format.

In addition to its use in configuration files, YAML is also commonly used for data serialization and as a format for storing and exchanging data between applications.

## See Also

- `gopkg.in/yaml.v2` documentation: https://pkg.go.dev/gopkg.in/yaml.v2
- Official YAML website: https://yaml.org/
- Additional resources for working with YAML in Go: https://github.com/golang/go/wiki/YAML