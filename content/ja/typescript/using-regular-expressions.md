---
title:                "正規表現の使用"
date:                  2024-01-19
simple_title:         "正規表現の使用"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

正規表現は文字列をパターンで扱うための方法です。これにより、検索、置換、データの検証などが簡単かつ効率的に行えます。

## How to: (方法)

正規表現の基本的な使用法とTypeScriptでの具体例を示します。

```typescript
const text: string = "TypeScriptを学ぼう！2023年はプログラミングスキルの向上を目指そう。";

// 単語を検索
const pattern: RegExp = /\bTypeScript\b/;
console.log(pattern.test(text)); // true

// 数字を全て抽出
const numbers: RegExp = /\d+/g; 
console.log(text.match(numbers)); // ['2023']

// '年'で続く数字を置換
const newText: string = text.replace(/(\d+)年/, '$1/');
console.log(newText); // TypeScriptを学ぼう！2023/はプログラミングスキルの向上を目指そう。
```

## Deep Dive (深堀り)

正規表現は1960年代に発明され、UNIXのツールで幅広く使われました。同様の機能をもつ代替手段には、文字列関数やパーサーがありますが、複雑な文字列操作には正規表現が便利です。TypeScriptでは内部的にJavaScriptのRegExpオブジェクトを使用し、パターンマッチングを行っています。

## See Also (関連項目)

- MDN Web Docs: [正規表現](https://developer.mozilla.org/ja/docs/Web/JavaScript/Guide/Regular_Expressions)
- 正規表現のチュートリアル: [RegExr](https://regexr.com/)
- TypeScript Handbook: [Basic Types](https://www.typescriptlang.org/docs/handbook/basic-types.html)
