---
date: 2024-01-26 04:27:47.713033-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u6211\u4EEC\u5C06\u4F7F\u7528`XMLWriter`\u5E93\
  \u6765\u521B\u5EFAXML\u6587\u6863\uFF0C\u5E76\u4F7F\u7528`tinyxml2`\u5E93\u6765\u89E3\
  \u6790\u5B83\u3002\u9996\u5148\u901A\u8FC7Arduino IDE\u7684\u5E93\u7BA1\u7406\u5668\
  \u5B89\u88C5\u8FD9\u4E9B\u5E93\u3002 \u521B\u5EFA\u4E00\u4E2AXML\u6587\u6863\uFF1A\
  ."
lastmod: '2024-03-13T22:44:48.090448-06:00'
model: gpt-4-0125-preview
summary: "\u6211\u4EEC\u5C06\u4F7F\u7528`XMLWriter`\u5E93\u6765\u521B\u5EFAXML\u6587\
  \u6863\uFF0C\u5E76\u4F7F\u7528`tinyxml2`\u5E93\u6765\u89E3\u6790\u5B83\u3002\u9996\
  \u5148\u901A\u8FC7Arduino IDE\u7684\u5E93\u7BA1\u7406\u5668\u5B89\u88C5\u8FD9\u4E9B\
  \u5E93."
title: "\u5904\u7406XML"
weight: 40
---

## 如何操作：
我们将使用`XMLWriter`库来创建XML文档，并使用`tinyxml2`库来解析它。首先通过Arduino IDE的库管理器安装这些库。

创建一个XML文档：

```Arduino
#include <XMLWriter.h>

void setup() {
  Serial.begin(9600);
  
  XMLWriter xml(&Serial); // 使用串行输出
  
  xml.header();
  xml.tag("greeting").tag("text").text("Hello, world!").close().close();
  xml.flush();
}

void loop() {
}
```

解码一个XML字符串：

```Arduino
#include <tinyxml2.h>

tinyxml2::XMLDocument doc;
doc.Parse("<greeting><text>Hello, world!</text></greeting>");

tinyxml2::XMLElement* text = doc.FirstChildElement("greeting")->FirstChildElement("text");
if (text != nullptr) {
  Serial.println(text->GetText());
}
```

样例输出：

```
<greeting>
  <text>Hello, world!</text>
</greeting>
```

## 深入探索
XML，或可扩展标记语言，是一种标记语言，为在格式既可由人阅读又可由机器阅读的文档中编码定义了一套规则。它自90年代末就已存在，并在各个领域得到了广泛使用，尤其是在需要平台独立的数据交换的场合。Arduino有限的内存资源使得在其上处理XML比在PC上更具挑战性。因此，轻量级库至关重要。尽管由于其更简单的语法和更小的占用空间，JSON在数据交换中获得了普及，但XML仍然被广泛使用，尤其是在处理遗留系统或需要通过模式验证文档的应用程序时。Arduino上XML实现的关键是流解析，它分段读取文档以降低内存使用。

## 另请参阅
- [TinyXML-2库文档](https://leethomason.github.io/tinyxml2/)
- [Arduino JSON库](https://arduinojson.org/)，用于处理JSON数据的另一种选择。
- [W3Schools XML教程](https://www.w3schools.com/xml/)，用于一般的XML学习。
- [W3C XML规范](https://www.w3.org/XML/)，官方的XML标准和建议。
