---
title:                "Javascript: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ

プログラムにおいて、文字列の検索と置換を行うことは非常に重要です。テキストの検索と置換を行うことで、複数のファイルの中から特定のパターンを探したり、大量のデータを一括で修正することができます。また、タイプミスや翻訳ミスを自動的に修正することもできます。

## 方法

今回は、Javascriptを使用してテキストの検索と置換を行う方法をご紹介します。まずは、検索と置換を行うための基本的な方法から始めましょう。

まず、検索するテキストを指定します。例えば、"hello world"という文字列を検索する場合は、以下のように記述します。

```Javascript
var text = "hello world";
```

次に置換する文字列を指定します。例えば、"hello"を"こんにちは"に置換する場合は、以下のように記述します。

```Javascript
var newText = text.replace("hello", "こんにちは");
```

上記のコードを実行すると、元の文字列である"hello world"が"こんにちは world"に置換されます。

もちろん、複数の文字列を一括で置換することもできます。例えば、"hello"を"こんにちは"に、"world"を"世界"に置換する場合は、以下のように記述します。

```Javascript
var newText = text.replace("hello", "こんにちは").replace("world", "世界");
```

上記のコードを実行すると、元の文字列である"hello world"が"こんにちは 世界"に一括で置換されます。

さらに、正規表現を使用して置換することも可能です。例えば、特定のパターンに一致する文字列をすべて置換する場合には、以下のようなコードを記述します。

```Javascript
var newText = text.replace(/hello/g, "こんにちは");
```

上記のコードを実行すると、"hello"ではなく、文字列中のすべての"hello"が"こんにちは"に置換されます。

## 深堀り

以上のように、Javascriptを使用してテキストの検索と置換を行うことで、効率的に複数のファイルや大量のデータを修正することができます。また、正規表現を使用することで、さらに高度な置換を行うことができます。

しかし、注意点としては、置換を行う際に元の文字列が変更されることはないということです。つまり、上記のコードを実行しても、元の変数"text"の値は変わらず、新しい文字列が返されるだけであるということです。そのため、置換を行う際には、新しい変数を使用することをおすすめします。

## 参考記事

- [String.prototype.replace() - MDN Web Docs](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [JavaScript で文字列を置換する方法 - Qiita](https://qiita.com/takeharu/items/ed70c1a449f38bdc7cbe)