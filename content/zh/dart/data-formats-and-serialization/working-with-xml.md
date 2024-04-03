---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:34.247721-07:00
description: "Dart \u7684\u6807\u51C6\u5E93\u4E2D\u6CA1\u6709\u5185\u7F6E\u652F\u6301\
  \ XML \u5904\u7406\u7684\u529F\u80FD\uFF0C\u56E0\u6B64\u9700\u8981\u4F7F\u7528\u7B2C\
  \u4E09\u65B9\u5305\u3002\u4E00\u4E2A\u6D41\u884C\u7684\u5305\u662F `xml`\u3002\u8981\
  \u4F7F\u7528\u5B83\uFF0C\u4F60\u9996\u5148\u9700\u8981\u5C06\u5B83\u6DFB\u52A0\u5230\
  \u4F60\u7684 `pubspec.yaml` \u4E2D\uFF1A ```yaml dependencies: xml: ^5.0.0 // \u4F7F\
  \u7528\u53EF\u7528\u7684\u6700\u65B0\u7248\u672C ``` \u7136\u540E\uFF0C\u5728\u4F60\
  \u7684 Dart\u2026"
lastmod: '2024-03-13T22:44:47.451162-06:00'
model: gpt-4-0125-preview
summary: "Dart \u7684\u6807\u51C6\u5E93\u4E2D\u6CA1\u6709\u5185\u7F6E\u652F\u6301\
  \ XML \u5904\u7406\u7684\u529F\u80FD\uFF0C\u56E0\u6B64\u9700\u8981\u4F7F\u7528\u7B2C\
  \u4E09\u65B9\u5305\u3002\u4E00\u4E2A\u6D41\u884C\u7684\u5305\u662F `xml`\u3002\u8981\
  \u4F7F\u7528\u5B83\uFF0C\u4F60\u9996\u5148\u9700\u8981\u5C06\u5B83\u6DFB\u52A0\u5230\
  \u4F60\u7684 `pubspec."
title: "\u5904\u7406XML"
weight: 40
---

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
