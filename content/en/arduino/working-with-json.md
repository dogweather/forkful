---
title:                "Working with json"
html_title:           "Arduino recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Why
JSON is a popular data format for exchanging information between programs and devices. It is lightweight, easy to read and write, and widely supported by different programming languages. By learning how to work with JSON in Arduino, you can easily integrate your projects with other systems and devices.

## How To
To get started with using JSON in Arduino, you will need to install a library called "ArduinoJson". You can do this by navigating to "Tools" > "Manage Libraries" in the Arduino IDE and searching for "ArduinoJson". Install the latest version and you are ready to go.

To use this library, you need to include it in your code by adding this line at the top:
```Arduino
#include <ArduinoJson.h>
```

### Creating JSON data
To create JSON data, we use the `StaticJsonDocument` class from the library. Let's create a simple example where we store temperature and humidity data in JSON format. First, we define the capacity of the JSON document, which is the maximum size of the data we want to store. We will use a capacity of 200 bytes in this example.
```Arduino
StaticJsonDocument<200> doc;
```

Next, we can add data to this document using the `JsonObject` class. We can add multiple key-value pairs to represent our data. In this example, we will add the temperature and humidity values as keys and their corresponding values.
```Arduino
doc["temperature"] = 25.5;
doc["humidity"] = 50;
```

### Serializing JSON data
Once we have added the data to our document, we can serialize it into a JSON string using the `serializeJson()` function. We can then print this string to the Serial Monitor to view the output.
```Arduino
// Serialize JSON data
String jsonStr;
serializeJson(doc, jsonStr);

// Print to Serial Monitor
Serial.println(jsonStr);
```
The output should look something like this: `{"temperature":25.5,"humidity":50}`

### Parsing JSON data
To parse JSON data, we first need to receive the JSON string from a source, such as an HTTP request or a sensor. We can then use the `deserializeJson()` function to convert the string into a `StaticJsonDocument`. From there, we can access the data using `JsonObject` or `JsonArray` depending on the structure of the data.

Let's say we receive the following JSON string:
```Arduino
String receivedJson = "{\"temperature\":27.8,\"humidity\":60}";
```
We can then use the `deserializeJson()` function to convert it into a `StaticJsonDocument` called `data`:
```Arduino
StaticJsonDocument<200> data;
deserializeJson(data, receivedJson);
```

We can now access the data using `JsonObject` and the corresponding keys:
```Arduino
float temp = data["temperature"]; // temp variable will hold the value 27.8
float humidity = data["humidity"]; // humidity variable will hold the value 60
```

## Deep Dive
There are a few more advanced topics to keep in mind when working with JSON in Arduino:
- Use `DynamicJsonDocument` for larger or more complex data, as it can handle variable sizes and is more memory efficient.
- Use the `JsonArray` class for creating arrays in JSON.
- You can save JSON data to a file on the Arduino's internal SD card or external memory using the `writeFile()` function from the SPIFFS library.
- Take note of the memory limitations on your Arduino board when working with larger JSON data.

## See Also
- Official ArduinoJson library documentation: https://arduinojson.org/
- Tutorial on parsing JSON data in Arduino: https://randomnerdtutorials.com/decoding-and-encoding-json-with-arduino-or-esp8266/
- Forum discussions on working with JSON in Arduino: https://forum.arduino.cc/index.php?topic=537224.0