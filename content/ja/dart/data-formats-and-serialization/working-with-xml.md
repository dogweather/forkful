---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:36.620168-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.733022-06:00'
model: gpt-4-0125-preview
summary: "Dart\u3067XML\u3092\u6271\u3046\u3053\u3068\u306F\u3001XML\u30C9\u30AD\u30E5\
  \u30E1\u30F3\u30C8\u306E\u89E3\u6790\u3001\u30AF\u30A8\u30EA\u3001\u304A\u3088\u3073\
  \u5909\u66F4\u3068\u3044\u3063\u305F\u51E6\u7406\u3092\u542B\u3080\u30D7\u30ED\u30BB\
  \u30B9\u3067\u3001Web\u30B5\u30FC\u30D3\u30B9\u3001\u8A2D\u5B9A\u30D5\u30A1\u30A4\
  \u30EB\u3001\u307E\u305F\u306F\u30EC\u30AC\u30B7\u30FC\u30B7\u30B9\u30C6\u30E0\u3068\
  \u5BFE\u8A71\u3059\u308B\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306B\u3068\
  \u3063\u3066\u6B20\u304B\u305B\u306A\u3044\u3082\u306E\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u306F\u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\u3001\u30C7\
  \u30FC\u30BF\u4EA4\u63DB\u3001\u8A2D\u5B9A\u3001\u307E\u305F\u306F\u9060\u9694\u624B\
  \u7D9A\u304D\u547C\u3073\u51FA\u3057\u3092\u3001\u4EBA\u9593\u304C\u8AAD\u3081\u308B\
  \u4E0A\u306B\u30DE\u30B7\u30F3\u304C\u89E3\u6790\u53EF\u80FD\u306A\u69CB\u9020\u5316\
  \u3055\u308C\u305F\u968E\u5C64\u5F62\u5F0F\u3067\u5B9F\u73FE\u3057\u307E\u3059\u3002\
  ."
title: "\u300CXML\u3068\u306E\u4F5C\u696D\u300D"
weight: 40
---

## 何となぜ？

DartでXMLを扱うことは、XMLドキュメントの解析、クエリ、および変更といった処理を含むプロセスで、Webサービス、設定ファイル、またはレガシーシステムと対話するアプリケーションにとって欠かせないものです。プログラマはこれを行うことで、データ交換、設定、または遠隔手続き呼び出しを、人間が読める上にマシンが解析可能な構造化された階層形式で実現します。

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
