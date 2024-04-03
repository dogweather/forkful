---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:36.620168-07:00
description: "\u65B9\u6CD5: Dart\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306B\
  \u306FXML\u306E\u51E6\u7406\u3092\u30B5\u30DD\u30FC\u30C8\u3059\u308B\u6A5F\u80FD\
  \u304C\u7D44\u307F\u8FBC\u307E\u308C\u3066\u3044\u306A\u3044\u305F\u3081\u3001\u30B5\
  \u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u88FD\u306E\u30D1\u30C3\u30B1\u30FC\u30B8\u3092\
  \u4F7F\u7528\u3059\u308B\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\u3002`xml`\u306F\
  \u4EBA\u6C17\u306E\u3042\u308B\u30D1\u30C3\u30B1\u30FC\u30B8\u306E\u4E00\u3064\u3067\
  \u3059\u3002\u3053\u308C\u3092\u4F7F\u7528\u3059\u308B\u306B\u306F\u3001\u307E\u305A\
  `pubspec.yaml`\u306B\u8FFD\u52A0\u3059\u308B\u5FC5\u8981\u304C\u3042\u308A\u307E\
  \u3059\uFF1A."
lastmod: '2024-03-13T22:44:41.733022-06:00'
model: gpt-4-0125-preview
summary: "Dart\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306B\u306FXML\u306E\
  \u51E6\u7406\u3092\u30B5\u30DD\u30FC\u30C8\u3059\u308B\u6A5F\u80FD\u304C\u7D44\u307F\
  \u8FBC\u307E\u308C\u3066\u3044\u306A\u3044\u305F\u3081\u3001\u30B5\u30FC\u30C9\u30D1\
  \u30FC\u30C6\u30A3\u88FD\u306E\u30D1\u30C3\u30B1\u30FC\u30B8\u3092\u4F7F\u7528\u3059\
  \u308B\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\u3002`xml`\u306F\u4EBA\u6C17\u306E\
  \u3042\u308B\u30D1\u30C3\u30B1\u30FC\u30B8\u306E\u4E00\u3064\u3067\u3059\u3002\u3053\
  \u308C\u3092\u4F7F\u7528\u3059\u308B\u306B\u306F\u3001\u307E\u305A`pubspec.yaml`\u306B\
  \u8FFD\u52A0\u3059\u308B\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\uFF1A."
title: "\u300CXML\u3068\u306E\u4F5C\u696D\u300D"
weight: 40
---

## 方法:
Dartの標準ライブラリにはXMLの処理をサポートする機能が組み込まれていないため、サードパーティ製のパッケージを使用する必要があります。`xml`は人気のあるパッケージの一つです。これを使用するには、まず`pubspec.yaml`に追加する必要があります：

```yaml
dependencies:
  xml: ^5.0.0 // 利用可能な最新バージョンを使用する
```

次に、Dartファイルでパッケージをインポートします：

```dart
import 'package:xml/xml.dart' as xml;
```

**XMLの解析:**

次のようなXML文字列があるとします：

```xml
<String name="greeting">Hello, world!</String>
```

次のようにXMLを解析して読み取ることができます：

```dart
void parseXml(String xmlString) {
    final document = xml.XmlDocument.parse(xmlString);
    final String content = document.findElements('String').single.getAttribute('name');
    print(content); // 出力：greeting
}

void main() {
  final xmlString = '<String name="greeting">Hello, world!</String>';
  parseXml(xmlString);
}
```

**XMLドキュメントの作成:**

`xml`パッケージを使用して新しいXMLドキュメントを作成することは簡単です：

```dart
void createXml() {
  final builder = xml.XmlBuilder();
  builder.processing('xml', 'version="1.0"');
  builder.element('greeting', nest: () {
    builder.attribute('name', 'hello');
    builder.text('Hello, world!');
  });
  final xmlDocument = builder.buildDocument();
  print(xmlDocument.toXmlString(pretty: true));
}

void main() {
  createXml();
}
```

**出力**：

```xml
<?xml version="1.0"?>
<greeting name="hello">Hello, world!</greeting>
```

**XMLのクエリと変更:**

要素を見つけたり変更したりするには、XPathのようなメソッドを使用できます：

```dart
void modifyXml(String xmlString) {
    var document = xml.XmlDocument.parse(xmlString);
    var greeting = document.findAllElements('greeting').first;
    
    // 'name'属性の変更
    greeting.setAttribute('name', 'greeting_modified');
    
    // 新しい子要素の追加
    greeting.children.add(xml.XmlElement(xml.XmlName('message'), [], [xml.XmlText('Goodbye!')]));
    
    print(document.toXmlString(pretty: true));
}

void main() {
  final xmlString = '<greeting name="hello">Hello, world!</greeting>';
  modifyXml(xmlString);
}
```

**出力**：

```xml
<greeting name="greeting_modified">
  Hello, world!
  <message>Goodbye!</message>
</greeting>
```

これらの例では、DartでXMLを扱うための基本的な操作を示しています。`xml`パッケージを使用すると、アプリケーションの要件に合わせてXMLドキュメントを解析、作成、および操作することができます。
