---
title:                "文字列の連結"
html_title:           "Javascript: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## これは何？なぜ？
文字列の結合は、プログラマーにとってよく使われる基本的な機能です。文字列を結合すると、複数の文字列を一つにまとめることができます。これにより、より複雑な文字列を作成したり、文字列をより見やすく整形したりすることができます。

## 方法：
文字列を結合する方法は非常に簡単です。以下のように、文字列の後に「+」演算子を追加し、結合したい文字列を追加します。そして、結合した文字列を出力すると、結合された文字列が表示されます。

```Javascript
const string1 = "こんにちは";
const string2 = "私の名前は";
const result = string2 + string3;
console.log(result);
//出力結果：私の名前はこんにちは
```

または、文字列を結合する際には、文字列をテンプレートリテラルで囲む方法もあります。これはバッククォート（`）を使用し、文字列内で${}を使用して結合したい変数を挿入することで行います。

```Javascript
const name = "太郎";
console.log(`こんにちは、私の名前は${name}です。`);
//出力結果：こんにちは、私の名前は太郎です。
```

## 詳細を知る：
文字列の結合は、古くから存在する基本的なコンピューターサイエンスの機能です。今でも、プログラミングにおいて必要不可欠な機能ですが、文字列の結合には他の方法もあります。例えば、文字列の結合に使用される「+」演算子は、実際には数値の加算においても使用されるため、パフォーマンスの面では効率的ではありません。そのため、大量の文字列を結合する際には、配列を使用した方が良いとされています。

## 参考：
- [MDN Web Docs](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [W3Schools](https://www.w3schools.com/jsref/jsref_concat_string.asp)