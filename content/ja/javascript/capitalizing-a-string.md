---
title:    "Javascript: 文字列の大文字化"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングにおいて、文字列の先頭を大文字にすることは非常に一般的なタスクです。これは、文章や名前を適切に表記するために必要な作業であり、より読みやすい文を作成するためにも重要です。

## 手順

```Javascript
// サンプル文字列
let str = "hello world";

// 文字列を先頭の大文字に変換する関数
function capitalizeString(string) {
  // 文字列を配列に分割
  let array = string.split(" ");
  // 配列の各要素の先頭文字を大文字に変換
  for (let i = 0; i < array.length; i++) {
    array[i] = array[i].charAt(0).toUpperCase() + array[i].slice(1);
  }
  // 配列を文字列に結合して返す
  return array.join(" ");
}

// 関数を利用して文字列を先頭の大文字に変換
console.log(capitalizeString(str));

// 出力結果： Hello World
```

このように、`capitalizeString`という関数を作成し、`split()`メソッドを使って文字列を単語ごとに配列に分割し、`charAt()`メソッドと`toUpperCase()`メソッドを使って各単語の先頭文字を大文字に変換し、`join()`メソッドを使って再び文字列に結合します。そして、`capitalizeString`関数を呼び出し、任意の文字列を引数として渡すことで、先頭の大文字に変換した文字列を返します。

## 深堀り

`capitalizeString`関数では、`split()`メソッドと`join()`メソッドを使って文字列と配列を相互に変換することで、文字列を簡単に操作することができます。また、`charAt()`メソッドを使って文字列から特定の位置の文字を取得し、`toUpperCase()`メソッドを使って大文字に変換することで、文字列の操作がより柔軟になります。さらに、`for`ループを使うことで、配列の各要素に対して同じ処理を繰り返し行うことができます。

## 他にも見る

- [Javascriptで文字列を大文字に変換する方法](https://www.w3schools.com/jsref/jsref_touppercase.asp)
- [String.prototype.split()の使い方](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/split)
- [String.prototype.charAt()の使い方](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [String.prototype.toUpperCase()の使い方](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)