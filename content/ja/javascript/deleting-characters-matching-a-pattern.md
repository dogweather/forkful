---
title:                "パターンに一致する文字を削除する"
date:                  2024-01-20T17:42:45.860532-07:00
model:                 gpt-4-1106-preview
simple_title:         "パターンに一致する文字を削除する"

category:             "Javascript"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列から特定のパターンにマッチする文字を削除するとは、不要なデータや危険なコード片を取り除くこと。これによって、データのクリーニングや、ユーザー入力の検証を行うことができる。

## How to: (方法)
JavaScript でパターンにマッチする文字を削除する基本的な方法は `replace()` メソッドを使うことです。正規表現を用いてマッチさせたい文字を指定します。

```javascript
let originalString = "こんにちは、1234世界！";
let newString = originalString.replace(/[0-9]/g, '');
console.log(newString); // "こんにちは、世界！"
```

上記のコードでは、`replace()` メソッドを使って数字を削除しています。`/[0-9]/g` は「0から9までの数字をグローバルに検索する」という意味の正規表現です。

## Deep Dive (深掘り)
JavaScriptで文字を削除する機能は古くからあり、`replace()` メソッドは ECMAScript の初期バージョンから使用されています。しかし、ECMAScript 2015 (ES6) 以降、テンプレートリテラルやアロー関数などの新機能により、コードの書き方がより直感的になりました。

別の方法として、`split()` と `join()` メソッドを使うテクニックもあります。

```javascript
let originalString = "さようなら、5678世界！";
let newString = originalString.split('').filter(char => !/[0-9]/.test(char)).join('');
console.log(newString); // "さようなら、世界！"
```

このコードでは、文字列を一文字ずつ分解して数字以外の文字をフィルタリングし、再び結合しています。しかし `replace()` の方が簡潔で高速です。

## See Also (関連情報)
- [MDN Web Docs: replace()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN Web Docs: Regular Expressions](https://developer.mozilla.org/ja/docs/Web/JavaScript/Guide/Regular_Expressions)
- [MDN Web Docs: split()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/split)
- [MDN Web Docs: join()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Array/join)
- [ECMAScript 2015 (ES6) Features](https://github.com/lukehoban/es6features)

これらのリンク先では、`replace()` メソッドや正規表現、そして他の文字列操作メソッドについてさらに詳しく学ぶことができます。
