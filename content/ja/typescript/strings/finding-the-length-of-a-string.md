---
title:                "文字列の長さを求める"
aliases: - /ja/typescript/finding-the-length-of-a-string.md
date:                  2024-01-20T17:48:20.325176-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の長さを求める"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

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
