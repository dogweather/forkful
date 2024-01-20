---
title:                "文字列を小文字に変換する"
html_title:           "Arduino: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

日本の皆さん、こんにちわ！
今日はJavascriptで文字列を小文字に変換するやり方についてお話しましょう。簡潔で実用的なスタイルであまり難しくないから安心して読んでください。

## 何となぜ？
文字列を小文字に変換するとは、すべての大文字を対応する小文字に置き換える処理のことです。なぜこれをやるかというと、比較、検索、整列などの操作を大文字と小文字を区別しないで行いたいとき、この方法が役立ちます。

## こうやる
こちらが一番ですぐ活用できる方法とコード例です。

```Javascript
var str = "Hello World!";
var lowerCaseString = str.toLowerCase();
console.log(lowerCaseString);
// 输出: hello world!
```

`toLowerCase()`関数を使って文字列中の大文字を全て小文字に変換します。

## ディープダイブ
`toLowerCase()` はJavaScriptで初めて導入され、その後多くのプログラミング言語でも利用できるようになりました。ただし、同じ目的を果たす他の方法もあります。例えば、ASCIIコードを直接操作すれば大文字を小文字に変換できます。しかし、これは多くのエッジケースを扱うのが困難であるため、通常は `toLowerCase()` を利用します。

この関数は文字列の各文字に対してUnicode値に基づいた変換を行うため、特殊文字でも問題なく小文字に変換できます。

## 関連リンク
- [MozillaのtoLowerCase文書](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [JavaScriptで文字列を操作しよう](https://developer.mozilla.org/ja/docs/Web/JavaScript/Guide/Text_formatting)

今回は以上です。次回もJavaScriptについて簡潔にシェアしますので、お楽しみに！