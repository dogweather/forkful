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

## なぜ

なぜ私たちは文字列を結合するのに取り組むのか、その意義について最大2文で説明します。

## 方法

文字列の結合には、JavaScriptに組み込まれている `+` 演算子を使用します。次の例では、2つの文字列を結合する方法を示します。

```Javascript
let firstName = "太郎";
let lastName = "山田";
console.log(firstName + " " + lastName);
```
**出力：** 太郎 山田

また、ES6からは、テンプレート文字列を使用することもできます。これにより、変数を組み込んでより複雑な文字列を作成することができます。

```Javascript
let songName = "Let It Be";
let artist = "The Beatles";
console.log(`「${songName}」 by ${artist}`);
```
**出力：** 「Let It Be」 by The Beatles

## ディープダイブ

文字列の結合は、JavaScriptの日常的な操作の1つです。これは、HTML要素の作成や、データ処理、文字列操作などに役立ちます。また、文字列の結合には `concat()` メソッドや `join()` メソッドなど、さまざまなオプションがあります。詳細については、[MDNドキュメント](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/concat)を参照してください。

## See Also

この記事を読んでJavaScriptの文字列の結合について学びましたが、他にも役に立つ情報がたくさんあります。以下のリンクをチェックしてください。

- [JavaScript+による文字列の結合方法](https://www.sejuku.net/blog/19898)
- [文法が分かれば使い方も簡単！JavaScriptにおけるテンプレートリテラルの活用方法](https://techacademy.jp/magazine/25943)
- [始める前に知っておきたい基本的なJavaScriptの書き方と注意点](https://qiita.com/chihiro/items/d0074f2875f7cdb47cce)