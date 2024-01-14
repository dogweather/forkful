---
title:                "Javascript: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

## なぜ文字列をキャピタライズするのか

文字列のキャピタライズとは、最初の文字を大文字にすることを意味します。例えば、"hello"という文字列を"Hello"に変えることです。このような処理を行う理由は、文字列のフォーマットを統一させるためです。例えば、データベースから取得した名前を表示する際に、全ての名前を大文字で表示したいといった場合に使用されます。

## 方法

文字列をキャピタライズするには、一般的な方法として、組み込みのメソッドである`toUpperCase()`を使用します。以下の例をご覧ください。

```Javascript
let name = "japan";
let capitalized_name = name.toUpperCase();
console.log(capitalized_name); // JAPAN
```

また、複数の単語から成る文字列をキャピタライズする場合は、以下のように関数を定義して使用することもできます。

```Javascript
function capitalizeString(str) {
  let words_array = str.toLowerCase().split(" ");
  let capitalized_array = words_array.map(word => word.charAt(0).toUpperCase() + word.slice(1));
  return capitalized_array.join(" ");
}

let sentence = "hello world";
let capitalized_sentence = capitalizeString(sentence);
console.log(capitalized_sentence); // Hello World
```

## ディープダイブ

文字列をキャピタライズする方法は、見た目の統一だけではなく、プログラム上でも重要です。例えば、文字列を比較する際に大文字と小文字を区別しないようにするためにも、キャピタライズすることが必要です。また、多言語対応のアプリケーションで、特定の言語の文字列をキャピタライズする場合にも役立ちます。

## また見る

- [JavaScriptのUpperCaseメソッド](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [文字列をキャピタライズする方法](https://medium.com/@peterchang_82818/how-to-capitalize-the-first-letter-of-a-string-in-javascript-eb161e7085ba)
- [文字列操作に関するドキュメント](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String)