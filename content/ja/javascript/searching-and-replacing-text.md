---
title:                "テキストの検索と置換"
html_title:           "Javascript: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
今日、私たちが使うほとんど全てのプログラムはテキストベースであり、その中には多くの文が含まれています。これらの文には間違いや古い情報があることがあり、それらを手動で変更するのは非常に時間と手間がかかります。そこで、テキストの検索と置換は、プログラミングでよく使われる機能の一つです。

## How To
テキストの検索と置換を行うには、JavaScriptの組み込みメソッドである`replace()`を使用します。このメソッドは、検索する文字列と置換する文字列を引数として受け取り、検索された文字列が置換された新しい文字列を返します。

例えば、「Hello World」の中の「Hello」を「こんにちは」に置換したい場合、以下のようにコードを書きます。

```javascript
let str = "Hello World";
let newStr = str.replace("Hello", "こんにちは");
console.log(newStr); // こんにちは World
```

もし、検索する文字列が複数ある場合は、正規表現を使うことができます。正規表現は、検索するパターンを指定するための特殊な文字列です。以下のようにコードを書くことで、文字列内のすべての「a」を「@」に置換することができます。

```javascript
let str = "apple, banana, orange";
let newStr = str.replace(/a/g, "@");
console.log(newStr); // @pple, b@n@n@, or@nge
```

## Deep Dive
`replace()`メソッドは、デフォルトでは最初に見つかった文字列のみを置換します。しかし、`g`フラグを使うことで、すべてのマッチする文字列を置換することができます。また、正規表現では、パターンマッチングに加えて置換時により複雑な処理を行うこともできます。

また、JavaScript以外にも多くのプログラミング言語やテキストエディタなどでも、同様にテキストの検索と置換を行うことができます。そのため、これらの機能を学ぶことは、プログラミングの基本的なスキルの一つとなります。

## See Also
- [String.prototype.replace() - MDN Web Docs](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [正規表現チュートリアル - Qiita](https://qiita.com/KentaYamamoto/items/d72c6a7f79d5fc91ef58)
- [正規表現テスター - regex101](https://regex101.com/)