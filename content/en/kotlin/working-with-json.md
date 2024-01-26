---
title:                "Working with JSON"
html_title:           "Arduino recipe: Working with JSON"
simple_title:         "Working with JSON"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

JSON (JavaScript Object Notation) is a format for structuring data, used for storage and transmission. Programmers use it because it’s lightweight, readable, and easily parsed by many languages, including Kotlin.

## How to:

To work with JSON in Kotlin, you can use the `kotlinx.serialization` library. Here’s a simple example of serializing and deserializing a data class.

```Kotlin
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import kotlinx.serialization.encodeToString
import kotlinx.serialization.decodeFromString

@Serializable
data class User(val name: String, val age: Int)

fun main() {
    val json = Json { prettyPrint = true }
    val userData = User("John Doe", 30)
    
    // Serialize to JSON
    val jsonString = json.encodeToString(userData)
    println(jsonString)
    
    // Deserialize from JSON
    val userObj = json.decodeFromString<User>(jsonString)
    println(userObj)
}
```

Sample output:

```
{
    "name": "John Doe",
    "age": 30
}
User(name=John Doe, age=30)
```

## Deep Dive

JSON’s simple syntax has roots in JavaScript, but it’s now language-independent. Alternatives like XML are more verbose. When working with JSON in Kotlin, the `kotlinx.serialization` library handles the heavy lifting, automatically converting Kotlin objects to and from JSON with annotations. It supports complex data types and handles corner cases, but manually parsing JSON is also an option if you need tighter control.

## See Also

- Kotlin Serialization Guide: [https://kotlinlang.org/docs/serialization.html](https://kotlinlang.org/docs/serialization.html)
- JSON Introduction: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
