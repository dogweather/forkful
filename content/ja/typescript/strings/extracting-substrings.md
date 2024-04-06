---
date: 2024-01-20 17:46:53.874955-07:00
description: "How to: (\u3084\u308A\u65B9) \u6700\u521D\u306FJavaScript\u304B\u3089\
  \u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u62BD\u51FA\u30E1\u30BD\u30C3\u30C9\u3092\
  \u53D7\u3051\u7D99\u3044\u3060TypeScript\u3002`substring`, `slice`,\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:37:50.042896-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u6700\u521D\u306FJavaScript\u304B\u3089\u30B5\u30D6\
  \u30B9\u30C8\u30EA\u30F3\u30B0\u62BD\u51FA\u30E1\u30BD\u30C3\u30C9\u3092\u53D7\u3051\
  \u7D99\u3044\u3060TypeScript\u3002`substring`, `slice`, `substr`\u306E3\u3064\u306E\
  \u30E1\u30BD\u30C3\u30C9\u304C\u3042\u308A\u307E\u3059\u3002\u305F\u3060\u3057\u3001\
  `substr`\u306F\u975E\u63A8\u5968\u3067\u5C06\u6765\u306E\u30D0\u30FC\u30B8\u30E7\
  \u30F3\u3067\u524A\u9664\u3055\u308C\u308B\u53EF\u80FD\u6027\u304C\u3042\u308A\u307E\
  \u3059\u3002\u65B0\u3057\u3044\u30B3\u30FC\u30C9\u3067\u306F`substring`\u304B`slice`\u3092\
  \u4F7F\u3044\u307E\u3057\u3087\u3046\u3002"
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
