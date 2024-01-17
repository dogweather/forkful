---
title:                "文字列を小文字に変換する"
html_title:           "TypeScript: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列を小文字に変換することは、プログラマーが文字列を操作するときによく行われることです。大文字や小文字を区別しないようにするためや、データの整形を行うためなど、さまざまな理由があります。

## 方法：

TypeScriptで文字列を小文字に変換するには、 `toLowerCase()` メソッドを使用します。例えば、以下のように記述します。

```TypeScript
let str: string = "Hello World";
console.log(str.toLowerCase());
```

出力結果は、 `hello world` となります。

## 深く調べる：

文字列を小文字に変換するメソッドは、現代のプログラミング言語では非常に一般的なものです。しかし、以前の言語ではそれぞれ独自の方法で実現していました。例えば、C言語では `tolower()` という関数がありました。

また、文字列を小文字に変換する代替手段として、正規表現を使う方法もあります。しかし、この方法はパフォーマンスの面では `toLowerCase()` メソッドよりも劣ることが多いです。

文字列を小文字に変換する実装詳細については、Unicodeの規定に従って行われます。Unicodeは、文字やテキストをコンピュータで扱うための国際規格であり、文字とそれに対応する番号を結びつける仕組みを提供しています。

## 関連記事：

- [TypeScript Handbook: Strings](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [MDN web docs: toLowerCase()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)