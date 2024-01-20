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

## What & Why?
JSON (JavaScript Object Notation) is a lightweight data interchange format that is commonly used for transmitting data between a server and a client, making it popular in web development and data storage applications. It is a human-readable text format that is easy to understand and parse, making it a convenient choice for programmers.

## How to:
To use JSON in an Arduino project, you can take advantage of the built-in library called ArduinoJson. Here's an example of how to create a JSON object and print it to the serial monitor:

```
#include <ArduinoJson.h>		// include the library

void setup() {
	Serial.begin(9600);		// initialize serial communication
}

void loop() {
	StaticJsonDocument<200> jsonDoc;	// create a JSON document with a capacity of 200 bytes
	jsonDoc["temperature"] = 25;		// add a key-value pair to the JSON object
	jsonDoc["humidity"] = 50;
	serializeJson(jsonDoc, Serial);		// serialize the JSON object
	delay(1000);
}
```

The output on the serial monitor will look like this:

```
{"temperature":25,"humidity":50}
```

You can also parse a JSON string and access its values by using the ```parseObject()``` and ```["key"]``` methods respectively. This is useful when receiving JSON data from an external source.

```
#include <ArduinoJson.h>		// include the library

void setup() {
	Serial.begin(9600);		// initialize serial communication
}

void loop() {
	String jsonString = "{\"name\":\"John\",\"age\":30}";	// example JSON string
	StaticJsonDocument<200> jsonDoc;						// create a JSON document with a capacity of 200 bytes
	auto error = deserializeJson(jsonDoc, jsonString);	// parse the JSON string
	if (error) {											// check for parsing errors
		Serial.println("Parsing failed");
	} else {
		String name = jsonDoc["name"];	// access the value of "name"
		int age = jsonDoc["age"];		// access the value of "age"
		Serial.print("Name: ");
		Serial.println(name);
		Serial.print("Age: ");
		Serial.println(age);
	}
	delay(1000);
}
```

The output on the serial monitor will look like this:

```
Name: John
Age: 30
```

## Deep Dive:
JSON was created in 2001 by Douglas Crockford as an alternative to XML for data exchange between web services. It is based on the JavaScript programming language, making it easy to use in web development. As mentioned earlier, JSON is a lightweight format compared to XML which makes it more efficient for transmitting data over the internet.

However, there are other alternatives to JSON for data interchange such as XML, YAML, and CSV. Each has its own advantages and disadvantages, but JSON remains a popular choice for its simplicity and compatibility with web technologies.

The ArduinoJson library supports both the JSON standard (RFC 8259) and the JSON-like format used by MongoDB, making it flexible for different use cases. It also offers additional features like JSON web tokens and JSON pointers.

## See Also:
- [ArduinoJson Library Documentation](https://arduinojson.org/)
- [JSON Official Website](https://www.json.org/)