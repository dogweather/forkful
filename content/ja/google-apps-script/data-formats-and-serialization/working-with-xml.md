---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:44.315732-07:00
description: "\u65B9\u6CD5\uFF1A Google Apps Script\u306F\u3001XML\u30C7\u30FC\u30BF\
  \u3092\u6271\u3046\u305F\u3081\u306B`XmlService`\u3092\u63D0\u4F9B\u3057\u307E\u3059\
  \u3002\u4EE5\u4E0B\u3067\u306F\u3001XML\u6587\u5B57\u5217\u3092\u89E3\u6790\u3057\
  \u3001\u305D\u306E\u5185\u5BB9\u3092\u5909\u66F4\u3057\u3001\u65B0\u3057\u3044XML\u6587\
  \u5B57\u5217\u3092\u751F\u6210\u3059\u308B\u65B9\u6CD5\u3092\u793A\u3057\u307E\u3059\
  \u3002 XML\u6587\u5B57\u5217\u306E\u89E3\u6790\uFF1A."
lastmod: '2024-03-13T22:44:41.476829-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\u306F\u3001XML\u30C7\u30FC\u30BF\u3092\u6271\u3046\u305F\
  \u3081\u306B`XmlService`\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u3067\
  \u306F\u3001XML\u6587\u5B57\u5217\u3092\u89E3\u6790\u3057\u3001\u305D\u306E\u5185\
  \u5BB9\u3092\u5909\u66F4\u3057\u3001\u65B0\u3057\u3044XML\u6587\u5B57\u5217\u3092\
  \u751F\u6210\u3059\u308B\u65B9\u6CD5\u3092\u793A\u3057\u307E\u3059."
title: "XML\u3068\u306E\u4F5C\u696D"
weight: 40
---

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
