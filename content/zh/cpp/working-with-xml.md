---
title:                "处理XML"
date:                  2024-01-26T04:28:26.054350-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理XML"

category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/working-with-xml.md"
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
