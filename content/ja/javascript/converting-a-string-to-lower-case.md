---
title:    "Javascript: 文字列を小文字に変換する"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

なぜ文字列を小文字に変換するのか？プログラミングにおいて、特定の文字列を小文字に変換することは非常に便利です。例えば、入力フォームからのユーザーの名前を取得する場合、名前の大文字や小文字の区別が必要ない場合もあります。文字列を小文字に変換することで、入力された人の名前がどのような形式であっても、データが統一されて扱いやすくなります。

## やり方

文字列を小文字に変換する方法は簡単です。文字列オブジェクトに含まれる`toLowerCase()`メソッドを使用します。下記のコードを見てみましょう。

```Javascript
let name = "Amy";
console.log(name.toLowerCase()); // 出力結果: "amy"
```

上記の例では、変数`name`に"Amy"という文字列を代入し、`toLowerCase()`メソッドを使用して小文字に変換しています。変換された結果はコンソールに表示されます。

## 深堀り

このように、文字列を小文字に変換するメソッドは簡単に使えますが、実際にはどのように動いているのでしょうか？`toLowerCase()`メソッドは文字列の各文字を小文字に変換し、新しい文字列を返します。そのため、変換元の文字列は変更されません。また、変換前から小文字だった文字列の場合は変化はありません。

## 参考

- [String.prototype.toLowerCase() - MDN](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)