---
title:                "XMLの扱い方"
date:                  2024-01-26T04:32:52.181966-07:00
model:                 gpt-4-0125-preview
simple_title:         "XMLの扱い方"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/working-with-xml.md"
---

{{< edit_this_page >}}

## 何となぜ？

XMLを使用するということは、コードを使用してXMLコンテンツの解析、操作、生成を意味します。プログラマーがそれを行う理由は、XMLが設定ファイル、データ交換、およびWebサービスに広く使用されているためです。これは、XMLが人間が読める形式でありながら、機械が解析可能な特性を持つからです。

## 方法：

XMLの解析方法は以下の通りです：

```javascript
let parser = new DOMParser();
let xmlString = `<note>
                    <to>User</to>
                    <from>Author</from>
                    <heading>Reminder</heading>
                    <body>Don't forget me this weekend!</body>
                 </note>`;

let xmlDoc = parser.parseFromString(xmlString, "application/xml");
console.log(xmlDoc.getElementsByTagName('to')[0].childNodes[0].nodeValue);
// 出力: User
```

そしてXMLを生成するには：

```javascript
let xmlDocument = document.implementation.createDocument('', '', null);
let noteElement = xmlDocument.createElement('note');
noteElement.appendChild(xmlDocument.createElement('to')).textContent = 'User';
xmlDocument.appendChild(noteElement);
let serializer = new XMLSerializer();
let xmlString = serializer.serializeToString(xmlDocument);
console.log(xmlString);
// 出力: <note><to>User</to></note>
```

## 詳細解説

XMLはeXtensible Markup Languageの略で、90年代後半からあるデータ形式です。これは、人間と機械の両方が読めるドキュメントをエンコーディングするための一連のルールを定義します。歴史的に、XMLはその柔軟性と構造化された階層によって、SOAPのようなWebサービスや数多くの設定ファイルに選ばれてきました。

XMLの代替品には、JavaScriptでの使用のしやすさと軽量さで人気があるJSON（JavaScript Object Notation）があります。また、YAMLも人間に優しく設定ファイルによく用いられる選択肢として価値があります。

XMLは、JavaScriptではDOMParserとXMLSerializerインターフェイスを使用して実装されます。XML DOM（Document Object Model）によって、HTMLと同じようにXMLドキュメントをナビゲートし、編集することができます。JSONの台頭にもかかわらず、多くのレガシーシステムや特定の業界がデータ交換にそれを依然として頼っているため、XMLを理解することは重要です。

## 参照

- MDN Web Docs (XML 解析): https://developer.mozilla.org/en-US/docs/Web/API/DOMParser
- W3Schools (XML DOM チュートリアル): https://www.w3schools.com/xml/dom_intro.asp
- 「XMLとは何か？」: https://www.w3.org/XML/
