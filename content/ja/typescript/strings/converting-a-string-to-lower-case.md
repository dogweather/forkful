---
date: 2024-01-20 17:39:19.815018-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-03-11T00:14:15.340414-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
---

{{< edit_this_page >}}

## What & Why?
## なにを？そしてなぜ？

文字列を小文字に変換するとは、アルファベットが大文字で書かれている文字列を全て小文字にする処理のこと。この変換は、大文字小文字を区別せずにデータを比較したり、検索したりするときに使う。

## How to:
## 方法:

```TypeScript
// 文字列を小文字に変換する簡単な例:
let greeting: string = "Konnichiwa, Sekai!";
let lowerCaseGreeting: string = greeting.toLowerCase();

console.log(lowerCaseGreeting); // "konnichiwa, sekai!"
```

```TypeScript
// 大文字小文字混じりの文字列を扱う例:
let mixedCaseString: string = "TypeScript Wa Tanoshii!";
let lowerCasedString: string = mixedCaseString.toLowerCase();

console.log(lowerCasedString); // "typescript wa tanoshii!"
```

## Deep Dive
## 詳細情報:

文字列を小文字に変換する処理は、プログラミングの世界に古くから存在しています。最初は単純な文字列処理としてのみ用いられていましたが、今日ではデータの正規化や検索処理に欠かせないツールとなっています。

TypeScriptでは、`String.prototype.toLowerCase` メソッドを使用してこの変換を行います。このメソッドはECMAScriptの標準に基づき、Unicode標準に従って各文字を適切な小文字に変換します。

代替手段として、`String.prototype.toLocaleLowerCase` があり、これは特定の言語に特化した小文字変換を提供します。たとえばトルコ語では、大文字の 'I' は小文字の 'i' には変換せず 'ı' にします。しかし、ほとんどのケースでは `toLowerCase` で十分です。

変換の実装には、特定の文字コードの範囲を対象としたマッピングテーブルが使われることが一般的です。各文字コードに対応する小文字のコードを見つけ出し、新しい文字列を作成します。

## See Also
## 関連情報:

- MDN Web Docsの`String.prototype.toLowerCase`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase
- Unicode標準についての詳細情報: https://www.unicode.org/
- TypeScript公式ドキュメント: https://www.typescriptlang.org/docs/
