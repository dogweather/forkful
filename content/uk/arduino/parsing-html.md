---
title:                "Парсинг HTML"
date:                  2024-01-20T15:29:51.417835-07:00
html_title:           "Arduino: Парсинг HTML"
simple_title:         "Парсинг HTML"

category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Parsing HTML means dissecting a string of HTML code to extract data. Programmers do this to interact with web content in their applications.

## How to: (Як це зробити:)
```Arduino
#include <Ethernet.h>
#include <HttpClient.h>

EthernetClient ethClient;
HttpClient httpClient = HttpClient(ethClient);

const String webPageUrl = "http://example.com";

void setup() {
  Serial.begin(9600);
  // Initialize Ethernet
  if (Ethernet.begin(mac) == 0) {
    Serial.println("Failed to configure Ethernet using DHCP");
    for (;;);
  }
  // Fetch the web page
  httpClient.beginRequest();
  httpClient.get(webPageUrl);
  httpClient.endRequest();

  // Reading the response and parsing
  int statusCode = httpClient.responseStatusCode();
  String response = httpClient.responseBody();
  
  // Simple parsing for a tag
  int index = response.indexOf("<title>");
  if(index >= 0) {
    int endIdx = response.indexOf("</title>", index);
    String title = response.substring(index + 7, endIdx);
    Serial.print("Title: ");
    Serial.println(title);
  }
}

void loop() {
  // This sketch runs setup once and exits, nothing needed here
}
```
Sample output:
```
Title: Example Domain
```

## Deep Dive (Поглиблений Аналіз):
Parsing HTML on Arduino comes with constraints due to limited memory and CPU. Historically, people would opt for server-side parsing and send processed data to Arduino. Alternatives include using regular expressions, String functions or dedicated libraries like `ArduinoJson` for JSON data. However, HTML is parse-agnostic which makes it tricky—no guaranteed structure (unlike JSON/XML). This calls for careful string manipulation and conservative use of precious Arduino memory.

## See Also (Дивіться також):
- ArduinoJson Library: https://arduinojson.org/
- Arduino Ethernet library: https://www.arduino.cc/en/Reference/Ethernet
- HTTPClient Library: https://www.arduino.cc/en/Tutorial/LibraryExamples/HttpClient
- Regular Expressions in C++ (Arduino): https://www.cplusplus.com/reference/regex/
- Arduino String Reference: https://www.arduino.cc/reference/en/language/variables/data-types/string/
