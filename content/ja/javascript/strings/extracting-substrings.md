---
date: 2024-01-20 17:46:25.671922-07:00
description: "How to: (\u65B9\u6CD5) JavaScript\u3067\u30B5\u30D6\u30B9\u30C8\u30EA\
  \u30F3\u30B0\u3092\u62BD\u51FA\u3059\u308B\u4E3B\u306A\u65B9\u6CD5\u306F\u3001`substring()`,\
  \ `slice()`, \u305D\u3057\u3066 `substr()` \u306E3\u3064\u3067\u3059\u3002\u4EE5\
  \u4E0B\u306E\u30B5\u30F3\u30D7\u30EB\u30B3\u30FC\u30C9\u3092\u898B\u3066\u307F\u307E\
  \u3057\u3087\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.451997-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) JavaScript\u3067\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\
  \u3092\u62BD\u51FA\u3059\u308B\u4E3B\u306A\u65B9\u6CD5\u306F\u3001`substring()`,\
  \ `slice()`, \u305D\u3057\u3066 `substr()` \u306E3\u3064\u3067\u3059\u3002\u4EE5\
  \u4E0B\u306E\u30B5\u30F3\u30D7\u30EB\u30B3\u30FC\u30C9\u3092\u898B\u3066\u307F\u307E\
  \u3057\u3087\u3046\u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
weight: 6
---

## How to: (方法)
JavaScriptでサブストリングを抽出する主な方法は、`substring()`, `slice()`, そして `substr()` の3つです。以下のサンプルコードを見てみましょう。

```javascript
let text = "こんにちは、JavaScript!";

// substring()を使用
let sub1 = text.substring(6, 16);
console.log(sub1); // 出力: JavaScript

// slice()を使用
let sub2 = text.slice(6, 16);
console.log(sub2); // 出力: JavaScript

// substr()を使用（非推奨）
let sub3 = text.substr(6, 10);
console.log(sub3); // 出力: JavaScript
```

## Deep Dive (掘り下げ)
`substring()` と `slice()` は似ていますが、負のインデックスを扱う場合が異なります。`slice()` は負のインデックスをサポートし、文字列の末尾からの位置を参照します。一方、`substring()` は負の値を0として扱います。`substr()` は現在、非推奨とされ、将来的には削除される可能性があります。これらのメソッドはECMAScript規格の進化に伴い追加され、より柔軟に文字列処理を行えるようになりました。しかし、可読性や未来のコード互換性を考えると、`slice()` がより優れた選択肢と言えます。

## See Also (関連情報)
- MDN Web Docsによる`String.prototype.substring()`の解説: [MDN substring](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- MDN Web Docsによる`String.prototype.slice()`の解説: [MDN slice](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- MDN Web Docsでの`String.prototype.substr()`の非推奨についての議論: [MDN substr](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
