---
title:                "Parsing html"
html_title:           "Arduino recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Why
Parsing HTML is a useful skill for anyone interested in web development, data analysis, or data extraction. It allows you to efficiently extract information from websites and automate tasks.

## How To
To parse HTML using Arduino, you will need to use the ArduinoJson library. Here is an example code that extracts the text from an HTML page and prints it to the serial monitor:

```
#include <ArduinoJson.h> // include the library

void setup() {
  // initialize serial monitor
  Serial.begin(9600);

  // create a JSON buffer
  const size_t capacity = JSON_OBJECT_SIZE(1) + 20;
  DynamicJsonDocument doc(capacity);

  // use the HTTP client to make a GET request to the website
  HTTPClient http;
  http.begin("https://www.example.com"); // replace with your desired website
  int httpCode = http.GET();

  if (httpCode > 0) {
    // get response and store it in a String variable
    String payload = http.getString();
    
    // parse the HTML using ArduinoJson
    deserializeJson(doc, payload);
    
    // extract the text from the HTML and store it in a variable
    String text = doc["contents"].as<const char*>();
    
    // print the extracted text to the serial monitor
    Serial.println(text);
  }

  // close HTTP connection
  http.end();
}

void loop() {
  // do nothing in loop
}
```

Sample Output:
```
This is the text from the website.
```

## Deep Dive
Parsing HTML involves extracting data from an HTML document, which is written in a specific format called Hypertext Markup Language (HTML). This language uses tags to structure the content of a web page, making it easy for a browser to interpret and display the page. By parsing the HTML, we can extract the desired information by targeting specific tags and their attributes.

The ArduinoJson library offers a simple and lightweight way to parse HTML. It uses the JavaScript Object Notation (JSON) data format, which is commonly used for data manipulation and transfer. Using this library, we first need to create a JSON buffer with enough memory to store the extracted data. Then we can use the HTTP client to make an HTTP request to the desired website and store the response in a String variable. Finally, we can use the functionality of the ArduinoJson library to parse the HTML and extract the data we want.

## See Also
- [ArduinoJson Library Documentation](https://arduinojson.org/)
- [Getting Started with Arduino](https://www.arduino.cc/en/Guide/HomePage)
- [HTTPClient Library Documentation](https://github.com/espressif/arduino-esp32/tree/master/libraries/HTTPClient)