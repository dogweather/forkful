---
title:                "Javascript: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

JavaScriptのプログラムを発展させるには、文字列を小文字に変換することが必要です。これには多くの理由がありますが、主な理由は入力された文字列が大文字や混在した形式で入力される可能性があるため、コードで比較や処理する際に正確な結果を得るためです。

## 方法

```Javascript
const string = "Hello, WORLD!";
console.log(string.toLowerCase());
```

上記の例では、"Hello, WORLD!"という文字列が全て小文字の"hello, world!"に変換されて出力されます。

```Javascript
const sentence = "This iS A sAmple SentEnce.";
console.log(sentence.toLowerCase());
```

この例では、"This iS A sAmple SentEnce."という文字列が全て小文字の"this is a sample sentence."に変換されて出力されます。

## 詳細

文字列を小文字に変換する場合、大文字、小文字、記号の組合せ、スペースや数字など、様々な要素を考慮する必要があります。JavaScriptでは、`toLowerCase()`という組み込みのメソッドを使用することで簡単に文字列を小文字に変換できます。このメソッドは、文字列の各文字を小文字に変換し、新しい文字列として返します。文字列を処理する際には、常に小文字に変換することで、より正確な結果を得ることができます。

## 参考リンク

- [JavaScript ToLowerCase() Method](https://www.w3schools.com/jsref/jsref_tolowercase.asp)
- [Convert String to Lowercase](https://dev.to/dev0928/javascript-convert-string-to-lowercase-4gp0) 

## その他の情報

### See Also

リンクなし