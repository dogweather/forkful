---
title:                "处理XML"
date:                  2024-03-08T21:57:34.247721-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

# 什么 & 为什么？

在 Dart 中处理 XML 涉及解析、查询和修改 XML 文档，这对于与 Web 服务、配置文件或遗留系统交互的应用程序至关重要。程序员这样做是为了在结构化、层次化的格式中实现数据交换、配置或甚至远程过程调用，该格式既方便人阅读，又可由机器解析。

## 如何进行：

Dart 的标准库中没有内置支持 XML 处理的功能，因此需要使用第三方包。一个流行的包是 `xml`。要使用它，你首先需要将它添加到你的 `pubspec.yaml` 中：

```yaml
dependencies:
  xml: ^5.0.0 // 使用可用的最新版本
```

然后，在你的 Dart 文件中导入该包：

```dart
import 'package:xml/xml.dart' as xml;
```

**解析 XML：**

假设你有一个这样的 XML 字符串：

```xml
<String name="greeting">你好，世界！</String>
```

你可以按以下方式解析和读取 XML：

```dart
void parseXml(String xmlString) {
    final document = xml.XmlDocument.parse(xmlString);
    final String content = document.findElements('String').single.getAttribute('name');
    print(content); // 输出：greeting
}

void main() {
  final xmlString = '<String name="greeting">你好，世界！</String>';
  parseXml(xmlString);
}
```

**创建 XML 文档：**

使用 `xml` 包创建一个新的 XML 文档很简单：

```dart
void createXml() {
  final builder = xml.XmlBuilder();
  builder.processing('xml', 'version="1.0"');
  builder.element('greeting', nest: () {
    builder.attribute('name', 'hello');
    builder.text('你好，世界！');
  });
  final xmlDocument = builder.buildDocument();
  print(xmlDocument.toXmlString(pretty: true));
}

void main() {
  createXml();
}
```

**输出**：

```xml
<?xml version="1.0"?>
<greeting name="hello">你好，世界！</greeting>
```

**查询和修改 XML：**

要查找或修改元素，你可以使用类似 XPath 的方法：

```dart
void modifyXml(String xmlString) {
    var document = xml.XmlDocument.parse(xmlString);
    var greeting = document.findAllElements('greeting').first;
    
    // 修改 'name' 属性
    greeting.setAttribute('name', 'greeting_modified');
    
    // 添加一个新的子元素
    greeting.children.add(xml.XmlElement(xml.XmlName('message'), [], [xml.XmlText('再见！')]));
    
    print(document.toXmlString(pretty: true));
}

void main() {
  final xmlString = '<greeting name="hello">你好，世界！</greeting>';
  modifyXml(xmlString);
}
```

**输出**：

```xml
<greeting name="greeting_modified">
  你好，世界！
  <message>再见！</message>
</greeting>
```

这些示例展示了在 Dart 中处理 XML 的基本操作。通过使用 `xml` 包，你可以解析、创建和操纵 XML 文档，以满足应用程序的要求。
