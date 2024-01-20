---
title:                "Travailler avec yaml"
html_title:           "Kotlin: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

# Working with YAML in Kotlin: De-mystifying the Process

## What & Why?
YAML stands for "YAML Ain't Markup Language" and is a popular data serialization language used in software development. It provides a human-friendly syntax for creating and storing data in a structured format, making it easy to read and understand. Programmers often use YAML to configure applications, transfer data between systems, and store data in a persistent manner.

## How to:
### Creating a YAML File:
To create a YAML file in Kotlin, you can use the ```kotlin FileWriter``` class. First, import the necessary packages:
```
import java.io.File
import java.io.FileWriter
```
Next, create an instance of FileWriter with the name of the file as a parameter:
```
val file = File("example.yaml")
val writer = FileWriter(file)
```
Then, use the ```write()``` method to add data to the file:
```
writer.write("name: John Doe \n")
writer.write("age: 30 \n")
writer.write("occupation: Developer \n")
```
Finally, close the writer to save the changes and create the file:
```
writer.close()
```

### Reading from a YAML File:
To read data from a YAML file in Kotlin, you can use the ```kotlin Yaml``` class from the ```org.yaml``` package. First, import the necessary packages:
```
import org.yaml.snakeyaml.Yaml
import java.io.File
```
Next, create an instance of ```Yaml``` and parse the file:
```
val yaml = Yaml()
val data: Map<String, Any> = yaml.load(File("example.yaml").inputStream())
```
Then, access the data using the key-value pairs:
```
println(data["name"]) // output: John Doe
println(data["age"]) // output: 30
println(data["occupation"]) // output: Developer
```

## Deep Dive:
### Historical Context:
YAML was first developed in 2001 by Clark Evans, Ingy döt Net, and Oren Ben-Kiki. It was designed as a simpler alternative to XML with a focus on readability and ease of use. It has since become a popular choice for configuring software systems, especially in web and scripting languages like Python and Ruby.

### Alternatives:
Some alternatives to YAML include JSON, XML, and INI files. YAML stands out for its human-readable syntax, allowing developers to easily create and edit data files without using special tools.

### Implementation Details:
YAML in Kotlin is implemented using the Snakeyaml library, which is a YAML 1.1 parser and emitter for the Java Virtual Machine. It provides classes for parsing, loading, and writing YAML documents. The ```Yaml``` class provides methods for converting YAML data into objects, making it easy to work with in Kotlin.

## See Also:
To learn more about working with YAML in Kotlin, check out the following resources:
- [YAML Tutorial](https://www.tutorialspoint.com/yaml/)