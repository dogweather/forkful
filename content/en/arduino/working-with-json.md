---
title:                "Arduino recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Why

JSON (JavaScript Object Notation) is a lightweight data interchange format that has become increasingly popular in recent years. It is widely used in web development, but it also has many applications in the world of Arduino programming. Working with JSON allows you to easily store, transmit, and manipulate data in a standardized format, making it an essential skill for any Arduino enthusiast.

## How To

To work with JSON in Arduino, you will need to install a library called "ArduinoJSON". You can do this by going to the Libraries Manager in the Arduino IDE, searching for "ArduinoJSON", and clicking on "Install".

Once the library is installed, you can start using it in your code. First, you need to define a JSON buffer to hold your data. This can be done using the `StaticJsonBuffer` class, as shown below:

```Arduino
#include <ArduinoJson.h>

StaticJsonBuffer<200> jsonBuffer; // define a 200-byte buffer
```

Next, you can create a JSON object and add data to it using the `JsonObject` class. In the example below, we are creating a JSON object with two key-value pairs: "name" and "age".

```Arduino
JsonObject& json = jsonBuffer.createObject();
json["name"] = "John";
json["age"] = 25;
```

You can also add arrays, nested objects, and integers to your JSON object. After adding all the necessary data, you can convert your JSON object into a string using the `printTo()` function and send it over a serial connection or save it to a file.

```Arduino
String jsonString;
json.printTo(jsonString); // convert JSON object to string

Serial.println(jsonString); // send over serial
```

To retrieve data from a JSON string, you can use the `parseObject()` function from the `StaticJsonBuffer` class. It will return a `JsonObject` that you can access to get the values of each key.

```Arduino
JsonObject& json = jsonBuffer.parseObject(jsonString); // parse JSON string

String name = json["name"]; // retrieve name
int age = json["age"]; // retrieve age
```

## Deep Dive

Working with JSON in Arduino may seem complex, but with the help of the ArduinoJSON library, it becomes much simpler. The library handles all the formatting and encoding of data, allowing you to focus on manipulating the data you need.

One thing to keep in mind when using JSON in Arduino is the limited memory available. You need to carefully choose the size of your buffer to avoid overflowing the memory. You can also use dynamic buffers (`DynamicJsonBuffer`) instead of static ones if you are unsure of the buffer size you need.

To learn more about working with JSON in Arduino, you can check out the official ArduinoJSON documentation and explore its different features and functions.

## See Also

- ArduinoJSON library: https://arduinojson.org/
- DynamicJsonBuffer: https://arduinojson.org/v5/api/dynamicjsonbuffer/
- Official ArduinoJSON documentation: https://arduinojson.org/v5/api/jsonobject/