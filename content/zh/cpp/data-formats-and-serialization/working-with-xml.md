---
date: 2024-01-26 04:28:26.054350-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u81EA90\u5E74\u4EE3\u672B\u4EE5\u6765\
  \uFF0CXML\u5728Web\u670D\u52A1\u548C\u6570\u636E\u5B58\u50A8\u4E2D\u4E00\u76F4\u53D1\
  \u6325\u7740\u5173\u952E\u4F5C\u7528\u3002\u5C3D\u7BA1\u73B0\u5728JSON\u548CYAML\u5BF9\
  \u4E8E\u914D\u7F6E\u548C\u4EA4\u4E92\u64CD\u4F5C\u66F4\u4E3A\u5E38\u89C1\uFF0CXML\u5728\
  \u8BB8\u591A\u4F01\u4E1A\u7CFB\u7EDF\u4E2D\u4ECD\u7136\u5360\u636E\u91CD\u8981\u4F4D\
  \u7F6E\u3002\u5728C++\u4E2D\u89E3\u6790XML\u53EF\u80FD\u4F1A\u611F\u89C9\u5230\u65E7\
  \u5B66\u6821\u98CE\u683C\uFF0C\u8FD9\u6D89\u53CA\u624B\u52A8DOM/SAX\u89E3\u6790\u3002\
  \u5E78\u8FD0\u7684\u662F\uFF0C\u50CFTinyXML-\u2026"
lastmod: '2024-04-05T22:51:01.346259-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u81EA90\u5E74\u4EE3\u672B\u4EE5\u6765\uFF0C\
  XML\u5728Web\u670D\u52A1\u548C\u6570\u636E\u5B58\u50A8\u4E2D\u4E00\u76F4\u53D1\u6325\
  \u7740\u5173\u952E\u4F5C\u7528\u3002\u5C3D\u7BA1\u73B0\u5728JSON\u548CYAML\u5BF9\
  \u4E8E\u914D\u7F6E\u548C\u4EA4\u4E92\u64CD\u4F5C\u66F4\u4E3A\u5E38\u89C1\uFF0CXML\u5728\
  \u8BB8\u591A\u4F01\u4E1A\u7CFB\u7EDF\u4E2D\u4ECD\u7136\u5360\u636E\u91CD\u8981\u4F4D\
  \u7F6E\u3002\u5728C++\u4E2D\u89E3\u6790XML\u53EF\u80FD\u4F1A\u611F\u89C9\u5230\u65E7\
  \u5B66\u6821\u98CE\u683C\uFF0C\u8FD9\u6D89\u53CA\u624B\u52A8DOM/SAX\u89E3\u6790\u3002\
  \u5E78\u8FD0\u7684\u662F\uFF0C\u50CFTinyXML-2\u8FD9\u6837\u7684\u5E93\u7B80\u5316\
  \u4E86\u8FD9\u4E2A\u8FC7\u7A0B\u3002C++\u6CA1\u6709\u5185\u7F6E\u7684XML\u652F\u6301\
  \uFF1B\u50CFTinyXML-2\u3001pugixml\u6216Xerces\u8FD9\u6837\u7684\u5E93\u7B80\u5316\
  \u4E86\u56F0\u96BE\u90E8\u5206\u3002"
title: "\u5904\u7406XML"
weight: 40
---

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
