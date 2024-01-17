---
title:                "正規表現の使用"
html_title:           "TypeScript: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

「##  何のためなのか？ 」

正規表現を使うとはどういうことか、それともプログラマーがなぜそれを使うのかをふたつの文で説明します。

正規表現とは、文字列のパターンを検索したり、置換したりするための強力なツールです。プログラマーは、大量のテキストデータから必要な情報を抽出するために、正規表現を使用します。

「## 方法：」

まずは、TypeScriptで正規表現を使用するための基本的な構文を紹介します。それから、実際のコーディング例を示し、その出力を示します。

```TypeScript
// 正規表現を使って、文字列内の数字を抽出する
const myString: string = "今日の天気は23℃です";
const pattern = /\d+/g; // 数字のパターンにマッチする
const result = myString.match(pattern); // ["23"]
```

この例では、myStringという文字列内の数字を抽出するために正規表現を使用しています。まず、抽出したい要素のパターンを定義し、そのパターンに一致する箇所をresultという変数に格納しています。

```TypeScript
// 正規表現を使って、文字列内のタグを置換する
const htmlString: string = "<p>こんにちは</p>";
const pattern = /<p>(.*?)<\/p>/g; // pタグの中身にマッチする
const result = htmlString.replace(pattern, "こんにちは！"); // "<p>こんにちは！</p>"
```

この例では、htmlStringという文字列内のpタグの中身を「こんにちは！」に置換するために正規表現を使用しています。まず、置換したい要素のパターンを定義し、そのパターンに一致する箇所を置換文字列に置き換えています。

「## 詳細を調べる」

正規表現は、1960年代に誕生し、その後様々なプログラミング言語で使用されるようになりました。しかし、正規表現を使う代わりに、プログラマーは独自の文字列操作方法を作成することもできます。正規表現の実装には、詳細な文法ルールがありますが、この記事では紹介しません。詳細を知りたい場合は、このリンクを参考にしてください。

「## 関連リンク」

- 正規表現の基本構文：https://developer.mozilla.org/ja/docs/Web/JavaScript/Guide/Regular_Expressions
- TypeScriptでの正規表現の使用方法：https://www.typescriptlang.org/docs/handbook/regular-expressions.html
- 正規表現の文法ルール：https://www.regular-expressions.info/