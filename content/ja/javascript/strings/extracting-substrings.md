---
date: 2024-01-20 17:46:25.671922-07:00
description: "JavaScript\u3067\u306F\u3001\u6587\u5B57\u5217\u304B\u3089\u7279\u5B9A\
  \u306E\u90E8\u5206\u3092\u53D6\u308A\u51FA\u3059\u64CD\u4F5C\u3092\u300C\u30B5\u30D6\
  \u30B9\u30C8\u30EA\u30F3\u30B0\uFF08\u90E8\u5206\u6587\u5B57\u5217\uFF09\u306E\u62BD\
  \u51FA\u300D\u3068\u547C\u3073\u307E\u3059\u3002\u3053\u306E\u6280\u8853\u306F\u3001\
  \u30E6\u30FC\u30B6\u30FC\u5165\u529B\u306E\u51E6\u7406\u3084\u30C7\u30FC\u30BF\u306E\
  \u5F62\u5F0F\u3092\u5909\u66F4\u3059\u308B\u969B\u306B\u983B\u7E41\u306B\u4F7F\u308F\
  \u308C\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.660420-06:00'
model: gpt-4-1106-preview
summary: "JavaScript\u3067\u306F\u3001\u6587\u5B57\u5217\u304B\u3089\u7279\u5B9A\u306E\
  \u90E8\u5206\u3092\u53D6\u308A\u51FA\u3059\u64CD\u4F5C\u3092\u300C\u30B5\u30D6\u30B9\
  \u30C8\u30EA\u30F3\u30B0\uFF08\u90E8\u5206\u6587\u5B57\u5217\uFF09\u306E\u62BD\u51FA\
  \u300D\u3068\u547C\u3073\u307E\u3059\u3002\u3053\u306E\u6280\u8853\u306F\u3001\u30E6\
  \u30FC\u30B6\u30FC\u5165\u529B\u306E\u51E6\u7406\u3084\u30C7\u30FC\u30BF\u306E\u5F62\
  \u5F0F\u3092\u5909\u66F4\u3059\u308B\u969B\u306B\u983B\u7E41\u306B\u4F7F\u308F\u308C\
  \u307E\u3059\u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
weight: 6
---

## What & Why? (何となぜ？)
JavaScriptでは、文字列から特定の部分を取り出す操作を「サブストリング（部分文字列）の抽出」と呼びます。この技術は、ユーザー入力の処理やデータの形式を変更する際に頻繁に使われます。

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
