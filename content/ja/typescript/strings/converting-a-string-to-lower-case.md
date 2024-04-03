---
date: 2024-01-20 17:39:19.815018-07:00
description: "\u306A\u306B\u3092\uFF1F\u305D\u3057\u3066\u306A\u305C\uFF1F \u6587\u5B57\
  \u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\u3068\u306F\u3001\u30A2\
  \u30EB\u30D5\u30A1\u30D9\u30C3\u30C8\u304C\u5927\u6587\u5B57\u3067\u66F8\u304B\u308C\
  \u3066\u3044\u308B\u6587\u5B57\u5217\u3092\u5168\u3066\u5C0F\u6587\u5B57\u306B\u3059\
  \u308B\u51E6\u7406\u306E\u3053\u3068\u3002\u3053\u306E\u5909\u63DB\u306F\u3001\u5927\
  \u6587\u5B57\u5C0F\u6587\u5B57\u3092\u533A\u5225\u305B\u305A\u306B\u30C7\u30FC\u30BF\
  \u3092\u6BD4\u8F03\u3057\u305F\u308A\u3001\u691C\u7D22\u3057\u305F\u308A\u3059\u308B\
  \u3068\u304D\u306B\u4F7F\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.739919-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\
  \u3068\u306F\u3001\u30A2\u30EB\u30D5\u30A1\u30D9\u30C3\u30C8\u304C\u5927\u6587\u5B57\
  \u3067\u66F8\u304B\u308C\u3066\u3044\u308B\u6587\u5B57\u5217\u3092\u5168\u3066\u5C0F\
  \u6587\u5B57\u306B\u3059\u308B\u51E6\u7406\u306E\u3053\u3068\u3002\u3053\u306E\u5909\
  \u63DB\u306F\u3001\u5927\u6587\u5B57\u5C0F\u6587\u5B57\u3092\u533A\u5225\u305B\u305A\
  \u306B\u30C7\u30FC\u30BF\u3092\u6BD4\u8F03\u3057\u305F\u308A\u3001\u691C\u7D22\u3057\
  \u305F\u308A\u3059\u308B\u3068\u304D\u306B\u4F7F\u3046\u3002"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
weight: 4
---

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
