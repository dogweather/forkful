---
aliases:
- /zh/cpp/working-with-xml/
date: 2024-01-26 04:28:26.054350-07:00
description: "\u4F7F\u7528XML\u6D89\u53CA\u89E3\u6790\u3001\u521B\u5EFA\u548C\u64CD\
  \u4F5CXML\uFF08\u53EF\u6269\u5C55\u6807\u8BB0\u8BED\u8A00\uFF09\u6570\u636E\u3002\
  \u7A0B\u5E8F\u5458\u5904\u7406XML\u4EE5\u5904\u7406\u7ED3\u6784\u5316\u6570\u636E\
  \u4EA4\u6362\u3001\u914D\u7F6E\u7B49\u4EFB\u52A1\uFF0C\u8FD9\u5F52\u529F\u4E8E\u5176\
  \u5E73\u53F0\u4E2D\u7ACB\u7684\u7279\u6027\u3002"
lastmod: 2024-02-18 23:08:59.426756
model: gpt-4-0125-preview
summary: "\u4F7F\u7528XML\u6D89\u53CA\u89E3\u6790\u3001\u521B\u5EFA\u548C\u64CD\u4F5C\
  XML\uFF08\u53EF\u6269\u5C55\u6807\u8BB0\u8BED\u8A00\uFF09\u6570\u636E\u3002\u7A0B\
  \u5E8F\u5458\u5904\u7406XML\u4EE5\u5904\u7406\u7ED3\u6784\u5316\u6570\u636E\u4EA4\
  \u6362\u3001\u914D\u7F6E\u7B49\u4EFB\u52A1\uFF0C\u8FD9\u5F52\u529F\u4E8E\u5176\u5E73\
  \u53F0\u4E2D\u7ACB\u7684\u7279\u6027\u3002"
title: "\u5904\u7406XML"
---

{{< edit_this_page >}}

## 什么及为什么？
使用XML涉及解析、创建和操作XML（可扩展标记语言）数据。程序员处理XML以处理结构化数据交换、配置等任务，这归功于其平台中立的特性。

## 如何操作：
以下是使用TinyXML-2库解析XML的简单方法：

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    doc.Parse("<root><message>Hello, World!</message></root>");
    const char* content = doc.FirstChildElement("root")->FirstChildElement("message")->GetText();
    std::cout << content << std::endl;
    return 0;
}
```

示例输出：

```
Hello, World!
```

这是如何创建一个XML文件的方法：

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    auto* declaration = doc.NewDeclaration();
    doc.InsertFirstChild(declaration);
    auto* root = doc.NewElement("root");
    doc.InsertEndChild(root);
    auto* message = doc.NewElement("message");
    message->SetText("Hello, World!");
    root->InsertEndChild(message);
    doc.SaveFile("output.xml");
    return 0;
}
```

这将生成一个内容为：

```xml
<?xml version="1.0"?>
<root>
    <message>Hello, World!</message>
</root>
```

的XML文件`output.xml`。

## 深入探索
自90年代末以来，XML在Web服务和数据存储中一直发挥着关键作用。尽管现在JSON和YAML对于配置和交互操作更为常见，XML在许多企业系统中仍然占据重要位置。在C++中解析XML可能会感觉到旧学校风格，这涉及手动DOM/SAX解析。幸运的是，像TinyXML-2这样的库简化了这个过程。C++没有内置的XML支持；像TinyXML-2、pugixml或Xerces这样的库简化了困难部分。

## 另请参阅
- TinyXML-2文档：https://leethomason.github.io/tinyxml2/
- pugixml库：https://pugixml.org/
- Xerces-C++解析器：https://xerces.apache.org/xerces-c/
- W3C XML规范：https://www.w3.org/XML/
