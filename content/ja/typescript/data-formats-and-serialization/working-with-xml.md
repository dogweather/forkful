---
date: 2024-01-26 04:36:28.137899-07:00
description: "\u3069\u3046\u3084\u3063\u3066\uFF1A XML\u3001\u307E\u305F\u306FExtensible\
  \ Markup Language\u306F\u300190\u5E74\u4EE3\u5F8C\u534A\u304B\u3089\u5B58\u5728\u3057\
  \u3066\u3044\u307E\u3059\u3002\u305D\u306E\u81EA\u5DF1\u8A18\u8FF0\u7684\u306A\u6027\
  \u8CEA\u3068\u4EBA\u9593\u304C\u8AAD\u3081\u308B\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\
  \u306F\u3001RSS\u30D5\u30A3\u30FC\u30C9\u3001\u8A2D\u5B9A\u7BA1\u7406\u3001\u305D\
  \u3057\u3066Microsoft Office Open\u2026"
lastmod: '2024-04-05T21:53:42.702480-06:00'
model: gpt-4-0125-preview
summary: "\u305D\u308C\u3067\u3082\u3001XML\u306F\u6B7B\u3093\u3067\u3044\u307E\u305B\
  \u3093\u3002\u5927\u898F\u6A21\u306A\u4F01\u696D\u30B7\u30B9\u30C6\u30E0\u3084JSON\u306B\
  \u79FB\u884C\u3057\u3066\u3044\u306A\u3044\u6587\u66F8\u6A19\u6E96\u3067\u4F7F\u7528\
  \u3055\u308C\u3066\u3044\u307E\u3059\u3002TypeScript\u7528\u306E`xml2js`\u3084Python\u306E\
  `lxml`\u306E\u3088\u3046\u306A\u30C4\u30FC\u30EB\u306F\u3001\u30D7\u30ED\u30B0\u30E9\
  \u30DF\u30F3\u30B0\u306B\u304A\u3051\u308BXML\u306E\u64CD\u4F5C\u306E\u5FC5\u8981\
  \u6027\u304C\u7D9A\u3044\u3066\u3044\u308B\u3053\u3068\u3092\u8A3C\u660E\u3057\u3066\
  \u3044\u307E\u3059."
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
