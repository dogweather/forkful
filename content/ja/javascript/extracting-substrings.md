---
title:    "Javascript: 部分文字列を抽出する"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

##なぜ？

文字列から部分文字列を抜き出すことのメリットはたくさんあります。例えば、文字列内の特定の単語や文字の出現回数を数える場合や、必要な情報を抜き出して新しい文字列を作成する場合に便利です。また、文字列が長すぎて読みづらい場合に、特定の部分のみを取り出して表示することで見やすくすることもできます。

##抜き出し方の例

文字列から部分文字列を抜き出す方法はいくつかありますが、ここではJavaScriptの標準メソッドである`substring()`を使った例を見ていきましょう。

```Javascript
let str = "Hello world";
console.log(str.substring(0, 5)); // Output: "Hello"
console.log(str.substring(6, 11)); // Output: "world"
console.log(str.substring(3)); // Output: "lo world"
```

この例では、`substring()`メソッドに抜き出したい部分文字列の開始位置と終了位置を引数として渡しています。第三引数を省略することで、開始位置から文字列の最後までを抜き出すことができます。戻り値として抜き出した部分文字列が返されるため、そのままコンソールに表示することで確認することができます。

##深堀り

`substring()`メソッド以外にも、文字列から部分文字列を抜き出す方法はあります。例えば、`slice()`メソッドや正規表現を使う方法があります。また、抜き出した文字列を変数に格納することで、後の処理に活用することもできます。このように、抜き出し方や利用方法は様々なので、自分のアプリケーションに合った最適な方法を選ぶことが重要です。

##参考リンク

- [MDN Web Docs - substring()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN Web Docs - slice()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [正規表現入門](https://www.webprofessional.jp/regex-character-classes-beginner/)