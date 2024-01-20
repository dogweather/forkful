---
title:                "Parsing html"
html_title:           "Gleam recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? 

Parsing HTML is extracting data from HTML code. Programmers do it to manipulate, transform, or extract specifics from an HTML document.

## How to:

Let's dive into the coding details. Here's an example of a simple Arduino program using the HTML library to parse HTML titles:

```Arduino
#include <ESP8266WiFi.h>
#include <HTMLParser.h>

HTMLParser htmlParser;

// callback for found HTML tags
void htmlTagCallback (HTMLTag& tag) {
  if (tag.tagName == "TITLE" && !tag.isEndTag) {
    Serial.println (tag.innerText);
  }
}

void setup() {
  Serial.begin(115200);
  htmlParser.init("http://example.com");
  htmlParser.setHTTPTagCallback(htmlTagCallback);
  htmlParser.processHTML();
}

void loop() {
  // put your main code here, to run repeatedly:
}

```

The output would be the title of the HTTP page at "http://example.com". 

## Deep Dive

HTML parsing isn't a new concept. Originated in the days of Interactive Data Language (IDL), it's used to extract information from the IDL files. 

There are alternatives to parsing HTML in Arduino, though. For instance, JSON is lighter and easier to 'pick apart' if the data is available in this format.

Details about parsing HTML include the fact that the HTML parser reads HTML codes from the top going down, and when it finds a match (tag), it interprets it and executes the assigned action if any.

## See Also

For more tutorials, you may consider checking these out: 

- Official Arduino Website (https://www.arduino.cc/)
- Arduino Stack Exchange Forum (https://arduino.stackexchange.com/)
- Arduino Playground - General (https://playground.arduino.cc/Category:General/)