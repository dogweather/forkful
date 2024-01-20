---
title:                "解析HTML"
html_title:           "Clojure: 解析HTML"
simple_title:         "解析HTML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## 什么和为什么?

HTML解析是一种将HTML代码解构为其组成元素的方法，这样程序员可以更容易地处理和操控这些元素。编程者做这个是为了能够从HTML中抓取数据，或在需要的情况下更改HTML代码。

## 如何操作:

以下是在Arduino中解析HTML的一个例子。我们将使用一个叫做ArduinoJson的库。

```Arduino
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);
  
  const char* html = "<h1>你好世界!</h1>";
  DynamicJsonDocument doc(1024);
  deserializeHtml(doc, html);

  JsonObject root = doc.as<JsonObject>();
  Serial.println(root["h1"].as<char *>()); // 你好世界!
}

void loop() {
}
```
这个例子中，`deserializeHtml`函数便会把HTML代码解析为JSON对象。然后，你就可以像上面一样，通过访问JSON对象的方式来访问HTML元素了。

## 深入研究:

在HTML的早期版本中，HTML解析是一个相当琐碎和混乱的过程，因为需要通过编写大量的正则表达式或字符串操作来提取所需要的信息。今天，有了像ArduinoJson这样的库，HTML解析变得非常直接和简单。

在Arduino中，还有其他一些库可以帮助你进行HTML解析，例如htmlText和Arduino HTML Parser。你可以根据你的需求和偏好来选择。

在使用HTML解析库时，需要注意的一点是，HTML解析可能占用大量的内存。在制定解析策略时，最好预先考虑到你Arduino 设备的内存限制。

## 参考资料:

1. [ArduinoJson库官方文档](https://arduinojson.org/v6/doc/deserialization/)