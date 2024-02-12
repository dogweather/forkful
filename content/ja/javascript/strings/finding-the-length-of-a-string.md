---
title:                "文字列の長さを求める"
aliases:
- /ja/javascript/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:57.433244-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の長さを求める"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列の長さを見つけるとは、文字列の中の文字数を数えることです。プログラマーはデータのバリデーション、入力のサイズ制限、表示の整形などを行うためにこれを実行します。

## How to: (方法)
```javascript
let greeting = 'こんにちは';
console.log(greeting.length);  // 出力: 5

let emptyString = '';
console.log(emptyString.length);  // 出力: 0

let emoji = '👋';
console.log(emoji.length);  // 出力: 2 (注意: 一般的な文字と異なる長さ)
```

## Deep Dive (深掘り)
文字列の長さを求めるには `length` プロパティが使われます。これは歴史的に常にJavaScriptに組み込まれており、非常に単純です。しかし、絵文字や特殊文字など、サロゲートペアを含む文字列の場合、`.length` は期待した値を返さないかもしれません。これは JavaScript が UTF-16 を使うためで、サロゲートペアは2つのコードユニットで一つの文字を表します。

別の方法として、配列に変換して `Array.from()` や スプレッド構文 `...` を使うことができますが、より新しいECMAScriptの機能です。

```javascript
let complexEmoji = '👨‍👩‍👧';
console.log(complexEmoji.length);  // 出力: 8 (誤り)
console.log(Array.from(complexEmoji).length);  // 出力: 1 (正確)
```

実装の詳細として、`.length` プロパティは文字列の各要素がメモリに保持されるインデックス数を返します。文字列に変更を加えると、このプロパティは自動的に更新されます。

## See Also (関連情報)
- JavaScriptの公式ドキュメント: [MDN Web Docs](https://developer.mozilla.org/ja/docs/Web/JavaScript)
- サロゲートペアについて: [サロゲートペア (MDN)](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/length#unicode)
- 文字列操作に関する詳しい説明と例: [JavaScript.info](https://javascript.info/string)
