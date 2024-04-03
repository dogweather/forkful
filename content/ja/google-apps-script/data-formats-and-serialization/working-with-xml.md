---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:44.315732-07:00
description: "Google Apps\u2026"
lastmod: '2024-03-13T22:44:41.476829-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\u3067\u306EXML\u306E\u6271\u3044\u65B9\u306F\u3001\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u304CXML\u30C7\u30FC\u30BF\u3092\u89E3\u6790\u3001\
  \u64CD\u4F5C\u3001\u751F\u6210\u3059\u308B\u3053\u3068\u3092\u53EF\u80FD\u306B\u3057\
  \u3001\u3053\u308C\u306F\u30A6\u30A7\u30D6\u30B5\u30FC\u30D3\u30B9\u3084\u8A2D\u5B9A\
  \u306B\u4E0D\u53EF\u6B20\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3053\u306E\u30A2\u30D7\u30ED\u30FC\u30C1\u3092\u53D6\u308B\u3053\u3068\u3067\u3001\
  \u30EC\u30AC\u30B7\u30FC\u30B7\u30B9\u30C6\u30E0\u3068\u306E\u7D71\u5408\u3001\u30A6\
  \u30A7\u30D6\u30B9\u30AF\u30EC\u30A4\u30D4\u30F3\u30B0\u306E\u5B9F\u884C\u3001\u307E\
  \u305F\u306FJSON\u3088\u308A\u3082XML\u306B\u4F9D\u5B58\u3059\u308B\u6570\u591A\u304F\
  \u306EAPI\u3068\u306E\u901A\u4FE1\u3092\u884C\u3044\u307E\u3059\u3002."
title: "XML\u3068\u306E\u4F5C\u696D"
weight: 40
---

## 何となぜ？

Google Apps ScriptでのXMLの扱い方は、プログラマーがXMLデータを解析、操作、生成することを可能にし、これはウェブサービスや設定に不可欠です。プログラマーはこのアプローチを取ることで、レガシーシステムとの統合、ウェブスクレイピングの実行、またはJSONよりもXMLに依存する数多くのAPIとの通信を行います。

## 方法：

Google Apps Scriptは、XMLデータを扱うために`XmlService`を提供します。以下では、XML文字列を解析し、その内容を変更し、新しいXML文字列を生成する方法を示します。

XML文字列の解析：

```javascript
function parseXML() {
  var xmlString = '<root><child name="first">Hello</child><child name="second">World</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  var children = root.getChildren('child');
  Logger.log(children[0].getText()); // ログ： Hello
}
```

XMLを変更するには、新しい子要素を追加することが考えられます：

```javascript
function addNewChild() {
  var xmlString = '<root><child name="first">Hello</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  
  var newChild = XmlService.createElement('child').setText('World');
  root.addContent(newChild);
  
  var xml = XmlService.getPrettyFormat().format(document);
  Logger.log(xml);
  // 追加した子要素を含む新しいXML文字列をログに出力
}
```

最初からXML文字列を生成：

```javascript
function createXML() {
  var root = XmlService.createElement('root');
  var child = XmlService.createElement('child').setText('Hello World');
  root.addContent(child);
  
  var xml = XmlService.getPrettyFormat().format(XmlService.createDocument(root));
  Logger.log(xml);
  // 出力： <root><child>Hello World</child></root>
}
```

## 深堀り

歴史的にXML（eXtensible Markup Language）は、JSONが軽量な代替品として現れる前のデータ交換の事実上の標準でした。XMLの冗長な構文と厳格な解析モデルは、かさばるものの、堅牢なデータ形式を提供しました。Google Apps Scriptでは、`XmlService` APIはXMLデータの作成、解析、操作をカプセル化し、様々なレガシーシステム、SOAPウェブサービス、アプリケーションの設定ファイルなどにおけるその継続的な重要性を認めています。

JSONがそのシンプルさとJavaScriptとの使いやすさにより現代のウェブ開発で普及しているにもかかわらず、文書の検証や構造化された階層が重要な領域ではXMLが依然として関連性を持っています。ただし、特にウェブAPIに傾倒する新しいプロジェクトにおいては、その軽量さとJavaScriptとのシームレスな統合により、JSONはしばしばより実用的な選択肢となります。

Google Apps ScriptでのXMLの理解と取り扱いは、古いシステムや特定の企業APIとの統合が必要な環境で働く開発者にとっては不可欠です。しかし、新しいプロジェクトを始める際や柔軟性が重要な場合は、JSONのような代替品を選択する必要性を評価することが勧められます。
