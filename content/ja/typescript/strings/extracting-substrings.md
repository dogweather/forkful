---
date: 2024-01-20 17:46:53.874955-07:00
description: "\u6587\u5B57\u5217\u306E\u4E00\u90E8\u3092\u53D6\u308A\u51FA\u3059\u3053\
  \u3068\u3092\u300C\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u3092\u62BD\u51FA\u3059\
  \u308B\u300D\u3068\u8A00\u3044\u307E\u3059\u3002\u30C7\u30FC\u30BF\u306E\u4E00\u90E8\
  \u3092\u5207\u308A\u53D6\u308A\u3001\u8868\u793A\u3057\u305F\u308A\u51E6\u7406\u3057\
  \u305F\u308A\u3059\u308B\u305F\u3081\u306B\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3053\u308C\u3092\u3088\u304F\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.741585-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u4E00\u90E8\u3092\u53D6\u308A\u51FA\u3059\u3053\
  \u3068\u3092\u300C\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u3092\u62BD\u51FA\u3059\
  \u308B\u300D\u3068\u8A00\u3044\u307E\u3059\u3002\u30C7\u30FC\u30BF\u306E\u4E00\u90E8\
  \u3092\u5207\u308A\u53D6\u308A\u3001\u8868\u793A\u3057\u305F\u308A\u51E6\u7406\u3057\
  \u305F\u308A\u3059\u308B\u305F\u3081\u306B\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3053\u308C\u3092\u3088\u304F\u884C\u3044\u307E\u3059\u3002."
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
weight: 6
---

## How to: (やり方)
```TypeScript
// 文字列から部分文字列を取得する方法

const fullText: string = 'こんにちは、TypeScript!';

// substringメソッド
const sub1: string = fullText.substring(0, 5);
console.log(sub1);  // 出力: こんにちは

// sliceメソッド
const sub2: string = fullText.slice(7);
console.log(sub2);  // 出力: TypeScript!

// substrメソッド (非推奨、使わない方が良い)
const sub3: string = fullText.substr(7, 10);
console.log(sub3);  // 出力: TypeScript!
```

## Deep Dive (深掘り)
最初はJavaScriptからサブストリング抽出メソッドを受け継いだTypeScript。`substring`, `slice`, `substr`の3つのメソッドがあります。ただし、`substr`は非推奨で将来のバージョンで削除される可能性があります。新しいコードでは`substring`か`slice`を使いましょう。

`substring`と`slice`の違いは、主に引数に負の値を指定できるかどうかです。`slice`は負の値を使って後ろから文字を数えられますが、`substring`ではできません。また、`substring`メソッドは引数の順番が逆でも最小値を始点、最大値を終点と解釈します。

他の言語でも似たような関数があり、そこでの経験がTypeScriptでの理解に繋がるでしょう。

## See Also (関連情報)
- TypeScript公式ドキュメント: [https://www.typescriptlang.org/docs/](https://www.typescriptlang.org/docs/)
- MDN Web Docs (文字列): [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- String.prototype.slice() vs String.prototype.substring(): [https://stackoverflow.com/questions/2243824/what-is-the-difference-between-string-slice-and-string-substring](https://stackoverflow.com/questions/2243824/what-is-the-difference-between-string-slice-and-string-substring)
