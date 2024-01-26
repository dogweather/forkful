---
title:                "XMLの扱い方"
date:                  2024-01-26T04:36:28.137899-07:00
model:                 gpt-4-0125-preview
simple_title:         "XMLの扱い方"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/working-with-xml.md"
---

{{< edit_this_page >}}

## 何となぜ？
XMLを扱うということは、プログラミングを使用してXMLデータの解析、操作、および書き込みをすることを意味します。プログラマーは、異なるシステム間でのデータ交換、設定ファイル、またはXMLに依存するSOAPのような標準を扱う際にXMLを処理します。

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