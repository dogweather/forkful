---
date: 2024-01-26 03:42:54.336097-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.740523-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u53D6\u308A\u9664\
  \u304F\u3068\u3044\u3046\u306E\u306F\u3001\u30B3\u30FC\u30C9\u5185\u306E\u6587\u5B57\
  \u5217\u30EA\u30C6\u30E9\u30EB\u3092\u5B9A\u7FA9\u3059\u308B\u5468\u56F2\u306E\u5358\
  \u4E00\uFF08`'`\uFF09\u307E\u305F\u306F\u4E8C\u91CD\uFF08`\"`\uFF09\u5F15\u7528\u7B26\
  \u6587\u5B57\u3092\u5265\u304C\u3059\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\
  \u3002\u4F8B\u3048\u3070\u3001\u51FA\u529B\u306E\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\
  \u3001\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u306E\u7121\u5BB3\u5316\u3001\u307E\u305F\
  \u306F\u5F15\u7528\u7B26\u304C\u4E0D\u8981\u3067\u3042\u3063\u305F\u308A\u30A8\u30E9\
  \u30FC\u3092\u5F15\u304D\u8D77\u3053\u3059\u53EF\u80FD\u6027\u304C\u3042\u308B\u5834\
  \u5408\u306E\u89E3\u6790\u3084\u4FDD\u5B58\u306E\u6E96\u5099\u306A\u3069\u3001\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3044\u304F\u3064\u304B\u306E\u7406\u7531\u3067\
  \u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002."
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
weight: 9
---

## 何となぜ？
文字列から引用符を取り除くというのは、コード内の文字列リテラルを定義する周囲の単一（`'`）または二重（`"`）引用符文字を剥がすことを意味します。例えば、出力のフォーマット、ユーザー入力の無害化、または引用符が不要であったりエラーを引き起こす可能性がある場合の解析や保存の準備など、プログラマーはいくつかの理由でこれを行います。

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
