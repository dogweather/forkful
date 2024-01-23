---
title:                "解析HTML"
date:                  2024-01-20T15:30:03.925686-07:00
html_title:           "Bash: 解析HTML"
simple_title:         "解析HTML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (什么和为什么？)
解析HTML意味着从网页代码中提取有用信息。程序员这么做是为了自动化处理网页数据，比如获取温度、股票价格等。

## How to: (怎么做：)
在Arduino中，解析HTML可以用多种库来进行，这里以MiniXPath为例，因其简洁高效：

```Arduino
#include <Ethernet.h>
#include <MiniXPath.h>

EthernetClient client;
char website[] = "example.com";
int port = 80;

void setup() {
  Serial.begin(9600);
  if (Ethernet.begin(mac) == 0) {
    Serial.println("Failed to configure Ethernet using DHCP");
  }
  if(client.connect(website, port)) {
    client.println("GET / HTTP/1.1");
    client.println("Host: example.com");
    client.println("Connection: close");
    client.println();
  }
}

void loop() {
  if (client.available()) {
    String line = client.readStringUntil('\r');
    char* result = XPath.match("<title>", line.c_str());
    if (result) Serial.println(result);
  }

  if (!client.connected()) {
    client.stop();
  }
}
```

样本输出：
```
Arduino Project Hub
```

## Deep Dive (深入研究)
解析HTML历史悠久，但在资源受限的Arduino上可能较复杂。MiniXPath是一个轻量级解析库，专门为Arduino等小型装置设计。虽然功能有限，它不支持复杂的XPath查询，但适用于简单任务。其它库如HtmlParser和ArduinoJson也可以用于HTML解析，但可能需更多内存。

## See Also (另请参阅)
- Arduino Ethernet库参考: [https://www.arduino.cc/en/Reference/Ethernet](https://www.arduino.cc/en/Reference/Ethernet)
- ArduinoJson库: [https://arduinojson.org/](https://arduinojson.org/)
