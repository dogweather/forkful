---
aliases:
- /ja/javascript/finding-the-length-of-a-string/
date: 2024-01-20 17:47:57.433244-07:00
description: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u898B\u3064\u3051\u308B\u3068\
  \u306F\u3001\u6587\u5B57\u5217\u306E\u4E2D\u306E\u6587\u5B57\u6570\u3092\u6570\u3048\
  \u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\
  \u30FC\u30BF\u306E\u30D0\u30EA\u30C7\u30FC\u30B7\u30E7\u30F3\u3001\u5165\u529B\u306E\
  \u30B5\u30A4\u30BA\u5236\u9650\u3001\u8868\u793A\u306E\u6574\u5F62\u306A\u3069\u3092\
  \u884C\u3046\u305F\u3081\u306B\u3053\u308C\u3092\u5B9F\u884C\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:55.260734
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u898B\u3064\u3051\u308B\u3068\
  \u306F\u3001\u6587\u5B57\u5217\u306E\u4E2D\u306E\u6587\u5B57\u6570\u3092\u6570\u3048\
  \u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\
  \u30FC\u30BF\u306E\u30D0\u30EA\u30C7\u30FC\u30B7\u30E7\u30F3\u3001\u5165\u529B\u306E\
  \u30B5\u30A4\u30BA\u5236\u9650\u3001\u8868\u793A\u306E\u6574\u5F62\u306A\u3069\u3092\
  \u884C\u3046\u305F\u3081\u306B\u3053\u308C\u3092\u5B9F\u884C\u3057\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
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
