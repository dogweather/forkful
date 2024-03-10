---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:36.620168-07:00
description: "\u2026"
lastmod: '2024-03-09T21:06:06.423619-07:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u300CXML\u3068\u306E\u4F5C\u696D\u300D"
---

{{< edit_this_page >}}

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
