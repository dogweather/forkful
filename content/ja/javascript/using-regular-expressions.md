---
title:                "正規表現を使用する"
html_title:           "Javascript: 正規表現を使用する"
simple_title:         "正規表現を使用する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なに？どうして？

正規表現を使うことは、文字列の特定のパターンを見つけるための便利な方法です。プログラマーはこれを使用することで、大量のテキストデータを効率的に取り扱うことができ、アプリケーションやウェブサイトの様々な機能を実現することができます。

## 作り方：

正規表現は、文字列に対してマッチングや置換を行うためのパターンを記述するための特別な記法です。例えば、電話番号やメールアドレスのような特定の形式を持つ文字列を抽出したり、文字列内の特定の文字列を置換したりすることができます。以下は、正規表現を使用した例です。

```Javascript
// 電話番号をマッチングする例
let phoneNumber = "123-4567-8901";
let regex = /\d{3}-\d{4}-\d{4}/;
console.log(phoneNumber.match(regex)); // Output: 123-4567-8901

// 文字列の置換する例
let str = "Hello, World!";
let newStr = str.replace(/World/, "Universe");
console.log(newStr); // Output: Hello, Universe!
```

## 詳しい情報：

正規表現は1960年代に開発され、それ以来、プログラミング言語やテキストエディタなど様々なソフトウェアで広く使用されてきました。ただし、正規表現以外にもパターンマッチングやテキスト処理をするための選択肢が存在します。例えば、JavaScriptでは、「substr」や「includes」のような組み込みのメソッドを使用することもできます。

正規表現は、通常、次のような特別な文字を組み合わせてパターンを表現します。

- メタ文字: 意味を持つ特別な文字。例えば、「\d」は数字を表す。
- 文字クラス: パターン内の文字の範囲を示す。例えば、「[a-z]」は小文字のアルファベットを表す。
- 量指定子: マッチする文字列の長さを指定する。例えば、「\d{3}」は3桁の数字を表す。

詳細な仕様については、正規表現のドキュメントを参照してください。

## 関連情報：

- [正規表現のドキュメント(英語)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)