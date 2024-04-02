---
date: 2024-01-20 17:48:20.325176-07:00
description: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u898B\u3064\u3051\u308B\u3063\
  \u3066\u3069\u3046\u3044\u3046\u3053\u3068\uFF1F\u5358\u7D14\u306B\u3001\u6587\u5B57\
  \u5217\u304C\u4F55\u6587\u5B57\u3067\u69CB\u6210\u3055\u308C\u3066\u3044\u308B\u304B\
  \u6570\u3048\u308B\u3053\u3068\u3060\u3088\u3002\u306A\u3093\u3067\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u304C\u3053\u308C\u3092\u3084\u308B\u306E\uFF1F\u5165\u529B\u691C\
  \u8A3C\u3001\u30C6\u30AD\u30B9\u30C8\u51E6\u7406\u3001UI\u30C7\u30B6\u30A4\u30F3\
  \u3092\u4E0A\u624B\u306B\u3084\u308B\u305F\u3081\u3060\u306D\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.744358-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u898B\u3064\u3051\u308B\u3063\
  \u3066\u3069\u3046\u3044\u3046\u3053\u3068\uFF1F\u5358\u7D14\u306B\u3001\u6587\u5B57\
  \u5217\u304C\u4F55\u6587\u5B57\u3067\u69CB\u6210\u3055\u308C\u3066\u3044\u308B\u304B\
  \u6570\u3048\u308B\u3053\u3068\u3060\u3088\u3002\u306A\u3093\u3067\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u304C\u3053\u308C\u3092\u3084\u308B\u306E\uFF1F\u5165\u529B\u691C\
  \u8A3C\u3001\u30C6\u30AD\u30B9\u30C8\u51E6\u7406\u3001UI\u30C7\u30B6\u30A4\u30F3\
  \u3092\u4E0A\u624B\u306B\u3084\u308B\u305F\u3081\u3060\u306D\u3002"
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
weight: 7
---

## What & Why? (なにとなぜ？)
文字列の長さを見つけるってどういうこと？単純に、文字列が何文字で構成されているか数えることだよ。なんでプログラマーがこれをやるの？入力検証、テキスト処理、UIデザインを上手にやるためだね。

## How to (やり方):
```TypeScript
// 文字列をの長さを取得する基本例
let greeting: string = "こんにちは";
console.log(greeting.length);  // 出力：5

// 空白含む文字列の長さも正しく取得
let sentence: string = "TypeScript って楽しい！";
console.log(sentence.length);  // 出力：19
```
文字列のプロパティ`.length`で長さをすぐに知ることができる。簡単でしょ。

## Deep Dive (深堀り):
文字列の長さを知る方法はプログラミングの初期からある。だけど、意外と深いトピックだよ。

**歴史的背景**: 古い言語では文字列は配列と密接に関係していて、終端に特別な文字（例えばC言語の`'\0'`）を持っていた。文字列の長さを得るには、終端文字まで数を数える必要があったね。

**代替手段**: TypeScriptの`.length`以外に、RegExpやループを使う手もある。だけど、`.length`が一番手軽で速い。

**実装の詳細**: TypeScriptはJavaScriptを基盤にしているから、`.length`プロパティはECMAScript標準に沿ってる。文字コードが合っていない特殊なケースでは、`.length`が期待通り動かないこともあるよ。

```TypeScript
// 特殊なケース：結合文字列
let emoji: string = "👨‍👩‍👧";
console.log(emoji.length); // 出力：5 (期待した結果は1かも)
```

この例では、絵文字は複数のUnicodeスカラー値で構成されていて、`.length`はそれぞれの値を数えてしまうんだ。

## See Also (関連する情報源):
- [MDN - String.length](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [TypeScript Official Documentation](https://www.typescriptlang.org/docs/)
- [ECMA-262 (ECMAScript Language Specification)](https://www.ecma-international.org/publications-and-standards/standards/ecma-262/)
