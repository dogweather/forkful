---
title:                "「テキストの検索と置換」"
html_title:           "Gleam: 「テキストの検索と置換」"
simple_title:         "「テキストの検索と置換」"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ

テキストを検索して置換すると何ができるかを説明する

テキストを検索して置換することは、プログラミングでよく使用される重要なタスクです。例えば、大量のテキストファイルを処理して特定の文字列を見つけて置き換える必要がある場合や、ウェブサイトで複数のページを一括で更新する必要がある場合などに使用されます。この記事では、Gleamを使ってテキストの検索と置換をする方法を説明します。

## 使い方

検索と置換を実装する方法を説明します。まずは、Gleamの標準ライブラリである`String`モジュールをインポートします。^3'`^1

```
import gleam/string
```

次に、置換前の文字列と置換後の文字列を定義します。

```
let source = "Hello, world!"
let target = "こんにちは、世界！"
```

そして、`String.replace`関数を使って検索と置換をします。この関数は、検索したい文字列、置換する文字列、そして対象となる文字列を引数として受け取り、置換後の文字列を返します。

```
let result = String.replace("Hello", "こんにちは", source)
```

最後に、結果を出力します。

```
IO.print(result)
```

このコードを実行すると、`こんにちは、世界！`という文字列が出力されます。

## 深堀り

`String.replace`関数の詳細を見てみましょう。この関数は内部で正規表現を使用しているため、検索する文字列は正規表現として解釈されます。例えば、特定の文字列の前後にある空白を置換する場合は、正規表現を使って`"\\sHello\\s"`というように指定することができます。また、置換後の文字列には正規表現のキャプチャグループを使用することもできます。

## 続きを読む

Gleamの`String`モジュールには他にも便利な関数や型があります。詳しくは公式ドキュメントを参照してください。

* [Gleam公式ドキュメント](https://gleam.run/documentation/guide.html)
* [正規表現の基本理論](https://www.geeksforgeeks.org/introduction-to-regular-expressions-regex/)