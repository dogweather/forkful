---
date: 2024-01-26 04:27:47.713033-07:00
description: "\u5728Arduino\u4E0A\u5904\u7406XML\u6D89\u53CA\u89E3\u6790\u548C\u64CD\
  \u4F5CXML\u6570\u636E\uFF0C\u8FD9\u4E9B\u6570\u636E\u901A\u5E38\u6765\u81EA\u4E8E\
  \u7F51\u7EDCAPI\u6216\u914D\u7F6E\u6587\u4EF6\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u662F\u4E3A\u4E86\u4E0E\u4F7F\u7528XML\u8FDB\u884C\u6570\u636E\u4EA4\u6362\
  \u7684\u670D\u52A1\u96C6\u6210\uFF0C\u6216\u8005\u4EE5\u7ED3\u6784\u5316\u3001\u53EF\
  \u8BFB\u7684\u683C\u5F0F\u5B58\u50A8\u6570\u636E\u3002"
lastmod: '2024-03-13T22:44:48.090448-06:00'
model: gpt-4-0125-preview
summary: "\u5728Arduino\u4E0A\u5904\u7406XML\u6D89\u53CA\u89E3\u6790\u548C\u64CD\u4F5C\
  XML\u6570\u636E\uFF0C\u8FD9\u4E9B\u6570\u636E\u901A\u5E38\u6765\u81EA\u4E8E\u7F51\
  \u7EDCAPI\u6216\u914D\u7F6E\u6587\u4EF6\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\
  \u662F\u4E3A\u4E86\u4E0E\u4F7F\u7528XML\u8FDB\u884C\u6570\u636E\u4EA4\u6362\u7684\
  \u670D\u52A1\u96C6\u6210\uFF0C\u6216\u8005\u4EE5\u7ED3\u6784\u5316\u3001\u53EF\u8BFB\
  \u7684\u683C\u5F0F\u5B58\u50A8\u6570\u636E\u3002"
title: "\u5904\u7406XML"
weight: 40
---

## 什么 & 为什么？
在Arduino上处理XML涉及解析和操作XML数据，这些数据通常来自于网络API或配置文件。程序员这样做是为了与使用XML进行数据交换的服务集成，或者以结构化、可读的格式存储数据。

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
