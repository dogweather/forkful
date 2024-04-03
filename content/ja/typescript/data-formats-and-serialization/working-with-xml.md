---
date: 2024-01-26 04:36:28.137899-07:00
description: "XML\u3092\u6271\u3046\u3068\u3044\u3046\u3053\u3068\u306F\u3001\u30D7\
  \u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u3092\u4F7F\u7528\u3057\u3066XML\u30C7\u30FC\
  \u30BF\u306E\u89E3\u6790\u3001\u64CD\u4F5C\u3001\u304A\u3088\u3073\u66F8\u304D\u8FBC\
  \u307F\u3092\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u7570\u306A\u308B\u30B7\u30B9\u30C6\u30E0\
  \u9593\u3067\u306E\u30C7\u30FC\u30BF\u4EA4\u63DB\u3001\u8A2D\u5B9A\u30D5\u30A1\u30A4\
  \u30EB\u3001\u307E\u305F\u306FXML\u306B\u4F9D\u5B58\u3059\u308BSOAP\u306E\u3088\u3046\
  \u306A\u6A19\u6E96\u3092\u6271\u3046\u969B\u306BXML\u3092\u51E6\u7406\u3057\u307E\
  \u3059\u3002"
lastmod: '2024-03-13T22:44:41.792790-06:00'
model: gpt-4-0125-preview
summary: "XML\u3092\u6271\u3046\u3068\u3044\u3046\u3053\u3068\u306F\u3001\u30D7\u30ED\
  \u30B0\u30E9\u30DF\u30F3\u30B0\u3092\u4F7F\u7528\u3057\u3066XML\u30C7\u30FC\u30BF\
  \u306E\u89E3\u6790\u3001\u64CD\u4F5C\u3001\u304A\u3088\u3073\u66F8\u304D\u8FBC\u307F\
  \u3092\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u7570\u306A\u308B\u30B7\u30B9\u30C6\u30E0\u9593\
  \u3067\u306E\u30C7\u30FC\u30BF\u4EA4\u63DB\u3001\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\
  \u3001\u307E\u305F\u306FXML\u306B\u4F9D\u5B58\u3059\u308BSOAP\u306E\u3088\u3046\u306A\
  \u6A19\u6E96\u3092\u6271\u3046\u969B\u306BXML\u3092\u51E6\u7406\u3057\u307E\u3059\
  \u3002."
title: "XML\u306E\u6271\u3044\u65B9"
weight: 40
---

## どうやって：
```TypeScript
import { parseString } from 'xml2js';

// サンプルXML
const xml = `<note>
                <to>User</to>
                <from>Author</from>
                <heading>Reminder</heading>
                <body>会議を忘れないでください！</body>
             </note>`;

// XMLをJSONに解析
parseString(xml, (err, result) => {
    if(err) throw err;
    console.log(result);
});

// 解析が成功したと仮定すると、出力は次のようになるかもしれません：
// { note:
//    { to: ['User'],
//      from: ['Author'],
//      heading: ['Reminder'],
//      body: ['会議を忘れないでください！'] } 
}
```

## ディープダイブ
XML、またはExtensible Markup Languageは、90年代後半から存在しています。その自己記述的な性質と人間が読めるフォーマットは、RSSフィード、設定管理、そしてMicrosoft Office Open XMLのようなオフィス文書フォーマットなど、さまざまなアプリケーションで早くからヒットしました。しかし、JSONと比べると冗長であり、時代は変わりました。JSONは、軽量でネイティブなJavaScriptの互換性のため、WebベースのAPIでスポットライトを浴びています。

それでも、XMLは死んでいません。大規模な企業システムやJSONに移行していない文書標準で使用されています。TypeScript用の`xml2js`やPythonの`lxml`のようなツールは、プログラミングにおけるXMLの操作の必要性が続いていることを証明しています。

TypeScriptはJSONのようにXMLを組み込んでサポートしていません。代わりに、ライブラリを使用します。`xml2js`はその一例です。XMLをJSONに変換し、JavaScriptの達人が扱いやすいデータにします。

## 参照
- [MDN Web DocsのXMLについて](https://developer.mozilla.org/en-US/docs/Web/XML/XML_introduction)
- [xml2js npmパッケージ](https://www.npmjs.com/package/xml2js)
- [W3SchoolsのXMLチュートリアル](https://www.w3schools.com/xml/)
