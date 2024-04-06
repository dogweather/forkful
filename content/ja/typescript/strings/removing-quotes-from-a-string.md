---
date: 2024-01-26 03:42:54.336097-07:00
description: "\u65B9\u6CD5\uFF1A TypeScript\u3067\u3042\u306A\u305F\u306E\u6587\u5B57\
  \u5217\u304B\u3089\u305D\u306E\u5384\u4ECB\u306A\u5F15\u7528\u7B26\u3092\u5207\u308A\
  \u96E2\u3059\u305F\u3081\u306E\u7121\u99C4\u306E\u306A\u3044\u30AC\u30A4\u30C9\u306F\
  \u3053\u3061\u3089\u3067\u3059\u3002"
lastmod: '2024-04-05T22:37:50.041634-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A TypeScript\u3067\u3042\u306A\u305F\u306E\u6587\u5B57\u5217\
  \u304B\u3089\u305D\u306E\u5384\u4ECB\u306A\u5F15\u7528\u7B26\u3092\u5207\u308A\u96E2\
  \u3059\u305F\u3081\u306E\u7121\u99C4\u306E\u306A\u3044\u30AC\u30A4\u30C9\u306F\u3053\
  \u3061\u3089\u3067\u3059\u3002"
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
weight: 9
---

## 方法：
TypeScriptであなたの文字列からその厄介な引用符を切り離すための無駄のないガイドはこちらです。

```typescript
// オプションA：正規表現を使用して単一または二重引用符を置換
function removeQuotes(input: string): string {
  return input.replace(/^["']|["']$/g, '');
}

console.log(removeQuotes(`"Quoted string"`)); // Quoted string
console.log(removeQuotes(`'Another one'`)); // Another one

// オプションB：始まりと終わりが異なる引用符を扱う
function removeMismatchedQuotes(input: string): string {
  return input.replace(/^(['"])(.*?)(?<!\1)\1$/, '$2');
}

console.log(removeMismatchedQuotes(`"Mismatched'`)); // "Mismatched'

// オプションC：複数種類の引用符をTrimming
function removeAllQuotes(input: string): string {
  return input.replace(/['"]+/g, '');
}

console.log(removeAllQuotes(`"'Mix'n'Match'"`)); // Mix'n'Match
```

## 深い考察
TypeScriptが登場するずっと前から、JavaScriptのコーダーたちは既に引用符の小細工に取り組んでおり、TypeScriptにおいても話はほぼ変わりません。時が経つにつれて、私たちが文字列を切り分ける方法も変わります。今日では、正規表現の筋力を利用して、面倒な文字列のスライスや他の手間のかかる方法を避けています。

上記の例でほとんどのニーズを満たすはずですが、引用符は複雑になり得ることを覚えておいてください。ネストされた、不一致の、そしてエスケープされた引用符は、あなたをつまずかせる待ち伏せているトリックスターです。これらのために、より洗練されたパターンやさらにはパーサーが必要となるかもしれません。

代替案は？`trim`や`trimStart` / `trimEnd`のようなメソッドを提供するlodashのようなライブラリを好む人もいます。これらは、スニップしたい文字を設定することで引用符を切り取るように調整することができます。

そして、TypeScript愛好家のために、型について忘れないでください。ここでは主に文字列を扱っていますが、ユーザー入力や解析を扱うときに型ガードやジェネリックを投入することで、コードを引用符をトリムするのと同じくらい安全に保つことができます。

## 参照
詳細情報については、これらのバーチャルホットスポットをチェックしてください：

- MDN Web Docsの正規表現（https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions）
- TypeScript公式ドキュメント（https://www.typescriptlang.org/docs/）
- あなたはLodash/Underscoreが不要です – 文字列ヘルパー（https://github.com/you-dont-need/You-Dont-Need-Lodash-Underscore#strings）
- Stack Overflow: 無数の開発者が引用符の災害と戦った戦場を横断（https://stackoverflow.com/）
