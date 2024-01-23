---
title:                "文字列の先頭を大文字にする"
html_title:           "C: 文字列の先頭を大文字にする"
simple_title:         "文字列の先頭を大文字にする"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ?)
文字列を大文字にするとは、全ての文字を大文字に変換することです。これは、タイトル、見出し、または特定の単語を強調するためによく行われます。

## How to: (方法)
```typescript
function capitalizeString(input: string): string {
  return input.toUpperCase();
}

// 使用例
const title = "typescriptの魅力";
console.log(capitalizeString(title));  // "TYPESCRIPTの魅力"
```

## Deep Dive (掘り下げ)
文字列の大文字化は、古くからある操作です。JavaScriptでは `.toUpperCase()` メソッドが使われ、TypeScriptでも同じです。代替手段として、`.map()` と ASCII コードを使用する方法もありますが、`.toUpperCase()` が最も直接的で速いです。内部的には、JavaScript の実行エンジンが各文字の大文字に対応する Unicode 値を見つけ、それに基づいて新しい文字列を生成します。

## See Also (関連情報)
- TypeScript公式ドキュメント: [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
- MDN Web Docs の `.toUpperCase()` メソッド: [String.prototype.toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
