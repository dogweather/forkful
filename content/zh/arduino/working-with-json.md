---
title:                "使用 json 进行编程"
html_title:           "Arduino: 使用 json 进行编程"
simple_title:         "使用 json 进行编程"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/working-with-json.md"
---

{{< edit_this_page >}}

# 为什么

你可能会想知道，为什么要学习使用JSON？JSON（JavaScript Object Notation）是一种轻量级的数据格式，通常用于在网络中传输数据。它可以帮助我们以一种易读易写的方式存储和共享数据，在Arduino编程中也有许多实际的用途。

# 怎样使用

使用JSON与Arduino编程时，我们需要一个库来帮助我们解析JSON数据。以下是一个基本的例子，用于从互联网获取JSON数据，并将其打印到串口监视器中：

```Arduino
#include <JsonParser.h> // 包含JSON库

// 创建一个JSON解析器对象
JsonParser parser;

void setup() {
  Serial.begin(9600); // 初始化串口通信
}

void loop() {
  // 创建一个JSON缓冲区，存储从互联网获取的数据
  char jsonBuffer[500];
  // 使用Arduino的HTTP Client库从URL中获取JSON数据
  HttpClient client;
  client.get("https://example.com/example.json", jsonBuffer, 500);

  // 调用JSON解析器的parse()函数，解析缓冲区中的数据
  JsonArray parsed = parser.parse(jsonBuffer);

  // 打印JSON数组中的每个值到串口监视器中
  for (int i = 0; i < parsed.size(); i++) {
    Serial.println(parsed[i].asString());
  }
}

```

如果一切顺利，你应该可以在串口监视器中看到从JSON数据中提取出的值。

## 深入了解

在Arduino编程中，我们经常需要从传感器或者其他设备中读取数据，然后将这些数据发送给互联网，或者从互联网获取数据。使用JSON，我们可以轻松地将这些数据以一种结构化的形式进行存储和传输。

在Arduino编程中，JSON解析器是一个重要的工具，它可以帮助我们更加有效地处理和处理数据。如果你想深入了解如何使用JSON库进行更复杂的操作，请查阅[Arduino官方文档](https://www.arduino.cc/en/Reference/ArduinoJson)。

# 参考资料

- [JSON简介](https://www.json.org/json-zh.html)
- [ArduinoJson文档](https://www.arduino.cc/en/Reference/ArduinoJson)