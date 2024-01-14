---
title:                "Kotlin recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## Why

JSON has become a widely used format for data exchange in web development and mobile applications. It is lightweight, human-readable, and supported by almost all programming languages. Working with JSON allows us to easily send and receive data from servers and APIs, making it an essential skill for developers to have.

## How To

To work with JSON in Kotlin, we can use the built-in library `org.json`. Here's a step-by-step guide on how to get started:

1. Import the `org.json` library into your project.

```Kotlin
import org.json.*
```

2. Create a `JSONObject` from a string using its constructor.

```Kotlin
val jsonString = "{"name": "John", "age": 25}"
val jsonObject = JSONObject(jsonString)
```

3. Access the values in the `JSONObject` using the `get` method.

```Kotlin
val name = jsonObject.get("name") // returns "John"
val age = jsonObject.get("age") // returns 25
```

4. To add a new key-value pair, use the `put` method.

```Kotlin
jsonObject.put("country", "USA")
```

5. Convert the `JSONObject` into a string using the `toString` method.

```Kotlin
val newJsonString = jsonObject.toString() // returns "{"name": "John", "age": 25, "country": "USA"}"
```

6. To convert a JSON string into a `JSONObject`, use the `JSONObject` constructor.

```Kotlin
val newJsonObject = JSONObject(newJsonString)
```

7. To handle nested JSON objects, use the `getJSONObject` method.

```Kotlin
val nestedJsonObject = jsonObject.getJSONObject("address") // assuming there is a key "address" with a nested JSON object
```

## Deep Dive

There are various approaches to working with JSON in Kotlin. We can use libraries like `GSON` or `Moshi` which provide more convenient ways to serialize and deserialize JSON data. Alternatively, we can also use the `kotlinx-serialization` library to convert JSON strings into Kotlin objects. This library also supports nested objects and custom mappings.

Another important aspect of working with JSON is handling errors. When trying to access a non-existent key, the `get` method returns a `null` value, which can lead to `NullPointerExceptions`. It is the developer's responsibility to handle such cases and ensure proper data validation.

## See Also

- [Kotlin JSON tutorial by Baeldung](https://www.baeldung.com/kotlin-json)
- [Kotlinx-serialization library documentation](https://github.com/Kotlin/kotlinx.serialization)