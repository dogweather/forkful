---
title:                "Working with json"
html_title:           "Kotlin recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## Why

JSON (JavaScript Object Notation) is a widely used data format for storing and exchanging information. It is particularly popular in web development and mobile app development. By learning how to work with JSON, you can access and manipulate data from different sources, making your applications more dynamic and powerful.

## How To

Working with JSON in Kotlin is made easy by the built-in support that the language offers. To get started, you need to have a basic understanding of JSON structure. Here's an example of a simple JSON object:

```Kotlin
val json = """
    {
        "name" : "John Doe",
        "age" : 30,
        "occupation" : "Software Engineer"
    }
```

To access the data in this JSON object, you can use the `JsonParser` class from the `kotlinx.serialization.json` package. Here's a code snippet that shows how to retrieve the value of the "name" field:

```Kotlin
val name = JsonParser.parseString(json).jsonObject["name"].toString()
println(name) //Output: "John Doe"
```

You can also easily convert a Kotlin object into JSON using the `Json.encodeToString()` function. For example:

```Kotlin
data class Person(val name: String, val age: Int)

val person = Person("Jane Smith", 25)
val json = Json.encodeToString(person)
println(json) //Output: {"name" : "Jane Smith", "age" : 25}
```

## Deep Dive

When working with more complex JSON structures, you can use the `jsonObject` and `jsonArray` properties to access nested objects and arrays. Here's an example:

```Kotlin
val json = """
    {
        "person" : {
            "name" : "Mark Johnson",
            "age" : 35,
            "address" : {
                "street" : "123 Main Street",
                "city" : "New York"
            },
            "hobbies" : ["reading", "hiking", "cooking"]
        }
    }
```

To retrieve the street address and first hobby from this JSON object, you can use the following code:

```Kotlin
val person = JsonParser.parseString(json).jsonObject["person"].jsonObject
val street = person["address"].jsonObject["street"].toString()
val firstHobby = person["hobbies"].jsonArray.first().toString()
println("$street loves $firstHobby") //Output: 123 Main Street loves reading
```

## See Also

- Kotlin JSON serialization: https://kotlinlang.org/docs/serialization-json.html
- Working with JSON in Android using Kotlin: https://developer.android.com/kotlin/first#working_with_json