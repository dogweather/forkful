---
title:                "正規表現を使用する"
html_title:           "TypeScript: 正規表現を使用する"
simple_title:         "正規表現を使用する"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ

正規表現を使用する理由は、テキストデータ内のパターンを検索、抽出、置換するためです。この機能により、テキスト処理をより効率的かつ正確に行うことができます。

## 使い方

正規表現を使用するには、まず "RegExp" オブジェクトを作成する必要があります。次に、検索や抽出などの操作を行うメソッドを使用することで、パターンを指定してテキストに対して処理を行うことができます。それでは、実際にTypeScriptで正規表現を使う例を見てみましょう。

```TypeScript
// 文字列がメールアドレスの形式かどうかをチェックする
const regex = new RegExp(/^[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}$/i);
 
const email = "example@domain.com";
const isValid = regex.test(email);
 
console.log(isValid); // true
```

この例では、テキストがメールアドレスの形式に合致しているかどうかをチェックしています。正規表現を使用することで、複数の文字列に対して同じパターンを適用することができるため、効率的にデータを処理することができます。

## ディープダイブ

正規表現にはさまざまなパターン記法があり、より複雑なパターンを指定することができます。たとえば、以下のような記法があります。

- `[]` : 文字の範囲を指定する
- `()` : グループを作成する
- `|` : OR条件を指定する
- `^` : 文字列の先頭を表す
- `$` : 文字列の末尾を表す

また、正規表現のメソッドには `exec()` や `matchAll()` など、さまざまなオプションがあります。詳しくは公式ドキュメントを参照してください。

## 参考リンク

- TypeScript 公式ドキュメント: https://www.typescriptlang.org/docs/handbook/regular-expressions.html
- 正規表現チュートリアル: https://www.w3schools.com/jsref/jsref_obj_regexp.asp
- マスターアップ TypeScript 正規表現入門: https://www.masterup.net/contents/how_to_use_regexp.html
- 正規表現を強力にする26のコマンド: https://qiita.com/shibukawa/items/d8bdb6a000d30583f6fc