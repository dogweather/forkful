---
title:                "Working with json"
html_title:           "Java recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/working-with-json.md"
---

{{< edit_this_page >}}

## Why

JSON has become a widely used format for data exchange due to its simplicity and flexibility. With its human-readable format and compatibility with most programming languages, JSON allows efficient communication between different systems and devices.

## How To

Working with JSON in Java is simple and straightforward. Here are some examples to get you started:

### Creating a JSON Object
```Java
JSONObject object = new JSONObject();
object.put("name", "John");
object.put("age", 28);
object.put("city", "New York");

System.out.println(object.toString());
```
Output:
```
{"name":"John","age":28,"city":"New York"}
```

### Parsing JSON Data
```Java
String jsonStr = "{\"name\":\"Amy\",\"age\":32}";

JSONObject jsonObject = new JSONObject(jsonStr);

String name = jsonObject.getString("name");
int age = jsonObject.getInt("age");

System.out.println(name + " is " + age + " years old.");
```
Output:
```
Amy is 32 years old.
```

### Writing JSON to File
```Java
JSONObject object = new JSONObject();
object.put("title", "My Book");
object.put("author", "Jane Smith");
object.put("year", 2020);

try (FileWriter file = new FileWriter("book.json")) {
    file.write(object.toString());
} catch (IOException e) {
    e.printStackTrace();
}
```

### Reading JSON from File
```Java
File file = new File("book.json");
try (FileReader reader = new FileReader(file)) {
    JSONObject object = (JSONObject) new JSONParser().parse(reader);

    String title = (String) object.get("title");
    String author = (String) object.get("author");
    int year = Integer.parseInt(object.get("year").toString());

    System.out.println("Title: " + title);
    System.out.println("Author: " + author);
    System.out.println("Year: " + year);
} catch (IOException | ParseException e) {
    e.printStackTrace();
}
```

## Deep Dive

JSON (JavaScript Object Notation) is a lightweight and platform-independent format for data exchange. It is based on a subset of JavaScript object literal syntax, making it easy to understand and use. By using key-value pairs, JSON allows for the representation of complex data structures that can be easily transmitted and decoded by different systems.

In Java, the "org.json" package provides easy-to-use classes for working with JSON data. The JSONObject class represents a JSON object, while the JSONArray class represents a JSON array. Both classes allow for the manipulation and retrieval of data using key-value pairs.

When parsing JSON data, it is important to handle exceptions and ensure that the JSON data is in the expected format. The "org.json.simple.parser" package provides a JSONParser class for this purpose. It allows for the conversion of JSON text into objects and vice versa.

Overall, working with JSON in Java is simple yet powerful, making it a widely used format for data exchange in various applications.

## See Also

- [JSON Tutorial by W3Schools](https://www.w3schools.com/js/js_json_intro.asp)
- [Oracle's Java JSON Tutorial](https://www.oracle.com/technical-resources/articles/java/jsont.html)
- [Introduction to JSON in Java with Examples by Baeldung](https://www.baeldung.com/java-json)