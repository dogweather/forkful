---
date: 2024-01-20 17:47:57.433244-07:00
description: "How to: (\u65B9\u6CD5) \u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\
  \u3081\u308B\u306B\u306F `length` \u30D7\u30ED\u30D1\u30C6\u30A3\u304C\u4F7F\u308F\
  \u308C\u307E\u3059\u3002\u3053\u308C\u306F\u6B74\u53F2\u7684\u306B\u5E38\u306BJavaScript\u306B\
  \u7D44\u307F\u8FBC\u307E\u308C\u3066\u304A\u308A\u3001\u975E\u5E38\u306B\u5358\u7D14\
  \u3067\u3059\u3002\u3057\u304B\u3057\u3001\u7D75\u6587\u5B57\u3084\u7279\u6B8A\u6587\
  \u5B57\u306A\u3069\u3001\u30B5\u30ED\u30B2\u30FC\u30C8\u30DA\u30A2\u3092\u542B\u3080\
  \u6587\u5B57\u5217\u306E\u5834\u5408\u3001`.length` \u306F\u671F\u5F85\u3057\u305F\
  \u5024\u3092\u8FD4\u3055\u306A\u3044\u304B\u3082\u3057\u308C\u307E\u305B\u3093\u3002\
  \u3053\u308C\u306F\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:42.153445-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B\
  \u306B\u306F `length` \u30D7\u30ED\u30D1\u30C6\u30A3\u304C\u4F7F\u308F\u308C\u307E\
  \u3059\u3002\u3053\u308C\u306F\u6B74\u53F2\u7684\u306B\u5E38\u306BJavaScript\u306B\
  \u7D44\u307F\u8FBC\u307E\u308C\u3066\u304A\u308A\u3001\u975E\u5E38\u306B\u5358\u7D14\
  \u3067\u3059\u3002\u3057\u304B\u3057\u3001\u7D75\u6587\u5B57\u3084\u7279\u6B8A\u6587\
  \u5B57\u306A\u3069\u3001\u30B5\u30ED\u30B2\u30FC\u30C8\u30DA\u30A2\u3092\u542B\u3080\
  \u6587\u5B57\u5217\u306E\u5834\u5408\u3001`.length` \u306F\u671F\u5F85\u3057\u305F\
  \u5024\u3092\u8FD4\u3055\u306A\u3044\u304B\u3082\u3057\u308C\u307E\u305B\u3093\u3002\
  \u3053\u308C\u306F JavaScript \u304C UTF-16 \u3092\u4F7F\u3046\u305F\u3081\u3067\
  \u3001\u30B5\u30ED\u30B2\u30FC\u30C8\u30DA\u30A2\u306F2\u3064\u306E\u30B3\u30FC\u30C9\
  \u30E6\u30CB\u30C3\u30C8\u3067\u4E00\u3064\u306E\u6587\u5B57\u3092\u8868\u3057\u307E\
  \u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
weight: 7
---

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
