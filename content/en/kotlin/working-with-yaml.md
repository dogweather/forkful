---
title:                "Working with yaml"
html_title:           "Kotlin recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why

If you're a developer working with configuration files, you've likely encountered YAML at some point. This human-readable data serialization language has gained popularity for its simple syntax and flexibility, making it a popular choice for configuration management in the tech industry.

## How To

To get started with YAML in your Kotlin project, you first need to add the dependency to your project's build.gradle file:

```Kotlin
dependencies {
    implementation("org.yaml:snakeyaml:1.28")
}
```

Once the dependency is added, you can start working with YAML in your code. Here's how you can create a YAML document and write it to a file:

```Kotlin
// create a YAML node
val rootNode = Yaml.createYamlDocumentBuilder().build()

// add key-value pairs
rootNode.addNode("name", "John Doe")
rootNode.addNode("age", 25)

// write the YAML document to a file
val file = File("person.yaml")
file.writeText(rootNode.toString())
```

The resulting YAML file will look like this:

```yaml
name: John Doe
age: 25
```

To read a YAML file into your Kotlin code, you can use the same Yaml class and its `load()` method:

```Kotlin
// read YAML file into a string
val yamlString = File("person.yaml").readText()

// load the YAML into a YamlDocument object
val document = Yaml.load(yamlString)

// access data using keys
println(document["name"]) // prints "John Doe"
println(document["age"]) // prints "25"
```

## Deep Dive

YAML stands for "YAML Ain't Markup Language" and was designed to be a human-friendly way to serialize data. It supports complex data structures like lists, maps, and even custom objects. 

One of the key features of YAML is its ability to use anchors and aliases, allowing you to reference one piece of data from another. This can be useful when creating a large YAML document with repeating sections.

Another advantage of YAML is its compatibility with various programming languages. You can easily parse YAML data in languages like Java, Python, and of course, Kotlin. This makes it a great choice for cross-platform projects.

## See Also

- [Official YAML website](https://yaml.org/)
- [Kotlin documentation on using YAML](https://kotlinlang.org/docs/tutorials/serialization.html#yaml)
- [SnakeYAML library on GitHub](https://github.com/snakeyaml-engine/snakeyaml)