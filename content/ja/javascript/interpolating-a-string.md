---
title:                "文字列の補間"
html_title:           "Javascript: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 何が？何故？

文字列の補間とは、よく使われる値を自動的に含めたり、動的な情報を表示させるためのテキストを作ることです。プログラマーはこのテクニックを使うことで、コードをより柔軟にし、簡潔に書くことができます。

## 方法：

```Javascript
let name = "太郎";
let greeting = `こんにちは、${name}さん！`;
console.log(greeting);
```

出力結果：

```
こんにちは、太郎さん！
```

下記のコードでは、テンプレートリテラル（` `）を使って、バックスラッシュを必要とせずに、変数 `item` の値を含んだ文字列を作成しています。

```Javascript
let item = "バナナ";
let price = 100;
let message = `今日は${item}が${price}円です。`;
console.log(message);
```

出力結果：

```
今日はバナナが100円です。
```

## 深く潜る：

文字列の補間は、古くからある`+`演算子を使った文字列の結合という方法に対する一つの代替手段です。しかし、テンプレートリテラルはよりシンプルかつ直感的な方法で、複雑な文字列を作成することができます。

また、文字列の補間は、変数や式を簡単に埋め込むことができるため、データやコードを動的に表示することができます。これにより、より動的で柔軟なプログラミングが可能になります。

## 関連リンク：

- [MDN: テンプレートリテラル](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/template_strings)
- [JavaScriptで文字列を結合する方法](https://www.digitalocean.com/community/tutorials/how-to-concatenate-strings-in-javascript)
- [JavaScript: 変数として文字列を展開する方法](https://stackoverflow.com/questions/3739207/variable-string-interpolation-javascript)