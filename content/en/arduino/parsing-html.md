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

## What & Why?
Parsing HTML is the process of analyzing and interpreting HTML code to identify its underlying structure and extract specific information from it. Programmers do this to automate tasks such as data extraction, web scraping, and web page manipulation.

## How to:
To parse HTML using Arduino, we will use the [ArduinoJson library](https://arduinojson.org/). Follow these steps to get started:
1. Install the library: Go to *Sketch* > *Include Library* > *Manage Libraries*. Search for "ArduinoJson" and click *Install*.
2. Create a new project: Go to *File* > *Examples* > *ArduinoJson* > *JsonParserExample*.
3. Modify the code: In the example code, replace the URL in line 20 with the webpage you want to parse.
4. Compile and upload: Verify your code and upload it to your Arduino board.
5. Check the serial monitor: After uploading, open *Tools* > *Serial Monitor*. You should see the parsed HTML data from the webpage you provided.

## Deep Dive:
(1) Parsing HTML has been around since the early days of the web, when it was mainly used to extract links and text from web pages. However, with the recent rise in web scraping and data extraction tasks, it has become an essential skill for many programmers.
(2) While ArduinoJson is a popular choice for parsing HTML with Arduino, other libraries like *ESP8266HTTPClient* and *WebParser* also offer similar functionalities.
(3) In the *JsonParserExample* code, the ```parseObject()``` function is used to extract data from the webpage in a JSON format. This function can be modified to extract specific elements such as *img* tags or *div* classes.

## See Also:
- [ArduinoJson library](https://arduinojson.org/)
- [ESP8266HTTPClient library](https://github.com/esp8266/Arduino/tree/master/libraries/ESP8266HTTPClient)
- [WebParser library](https://github.com/amicojeko/WebParser)