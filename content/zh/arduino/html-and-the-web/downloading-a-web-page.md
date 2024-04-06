---
date: 2024-01-20 17:43:31.174335-07:00
description: "\u5B9E\u73B0\u8FC7\u7A0B\uFF1A \u8FD9\u6BB5\u4EE3\u7801\u4F1A\u8FDE\u63A5\
  \u5230WiFi\uFF0C\u7136\u540E\u4ECE`http://example.com`\u4E0B\u8F7D\u7F51\u9875\u5185\
  \u5BB9\uFF0C\u5E76\u628A\u5B83\u6253\u5370\u51FA\u6765\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.357331-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u4E0B\u8F7D\u7F51\u9875"
weight: 42
---

## 实现过程：
```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "yourSSID";
const char* password = "yourPASSWORD";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Connecting to WiFi...");
  }

  Serial.println("Connected to WiFi");
  
  if (WiFi.status() == WL_CONNECTED) {
    HTTPClient http;
    http.begin("http://example.com"); //Your URL
    int httpCode = http.GET();

    if (httpCode > 0) {
      String payload = http.getString();
      Serial.println(httpCode);
      Serial.println(payload);
    } else {
      Serial.println("Error in HTTP request");
    }
    http.end();
  }
}

void loop() {
  // nothing here
}
```
这段代码会连接到WiFi，然后从`http://example.com`下载网页内容，并把它打印出来。

## 深入了解：
下载网页不是Arduino起初的设计目标，它在历史上主要用于与硬件交互。但随着ESP8266和ESP32这类带有WiFi功能的模块的出现，Arduino可以轻松地连接到Internet，进行数据交互。

除了上面展示的ESP8266HTTPClient库，还可以使用其他库，如WiFiClient或兼容的第三方库，来完成类似的任务。

更多实现细节包括处理不同的HTTP方法、安全连接（HTTPS）以及如何处理更大的数据流。考虑到数据的格式，有时你还需要使用JSON解析器，如ArduinoJson库，来把数据转换成可处理的格式。

## 参考链接：
- ESP8266WiFi库文档: https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html
- ESP8266HTTPClient库文档: https://arduino-esp8266.readthedocs.io/en/latest/esp8266httpclient/readme.html
- ArduinoJson库官方网站: https://arduinojson.org/
