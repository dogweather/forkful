---
title:                "Java recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/working-with-json.md"
---

{{< edit_this_page >}}

## Why

JSON (JavaScript Object Notation) is a popular data interchange format, commonly used in web development and API integration. It allows for efficient and compact data representation, making it a preferred choice for transferring data over the internet. If you are a Java developer, it is important to understand how to work with JSON to effectively communicate with other systems or applications.

## How To

To work with JSON in Java, we will use the ```JSONObject``` class from the ```org.json``` package. Let's start by creating a simple JSON object:

```Java
JSONObject user = new JSONObject();
user.put("name", "Tom");
user.put("age", 25);
```

Here, we have created a JSON object with two key-value pairs representing a user's name and age. We can access and print the values using the ```get()``` method:

```Java
System.out.println(user.get("name")); // Output: Tom
System.out.println(user.get("age")); // Output: 25
```

Next, let us convert this JSON object to a string using the ```toString()``` method:

```Java
String userString = user.toString();
System.out.println(userString); // Output: {"name":"Tom","age":25}
```

To parse a JSON string into a ```JSONObject```, we can use the ```JSONObject``` constructor:

```Java
String jsonString = "{\"fruit\":\"apple\",\"color\":\"red\"}";
JSONObject apple = new JSONObject(jsonString);
System.out.println(apple.get("fruit")); // Output: apple
System.out.println(apple.get("color")); // Output: red
```

We can also handle nested JSON objects and arrays using the same methods. For example:

```Java
JSONObject nestedObj = new JSONObject();
JSONArray fruits = new JSONArray();

fruits.put("apple");
fruits.put("orange");
fruits.put("banana");

nestedObj.put("fruits", fruits);

System.out.println(nestedObj.toString()); // Output: {"fruits":["apple","orange","banana"]}

```

## Deep Dive

One important thing to note when working with JSON in Java is that the ```JSONObject``` class does not preserve the order of elements, unlike in JavaScript. This is because it uses a ```HashMap``` to store the key-value pairs. As a workaround, we can use the ```JSONArray``` class to maintain the order of elements.

Additionally, when handling large or complex JSON data, it is recommended to use a library such as ```Gson``` or ```Jackson```, which provide more robust and efficient methods for dealing with JSON.

## See Also

- Official documentation for [JSONObject class](https://docs.oracle.com/javaee/7/api/javax/json/JsonObject.html)
- [Gson library](https://github.com/google/gson)
- [Jackson library](https://github.com/FasterXML/jackson)