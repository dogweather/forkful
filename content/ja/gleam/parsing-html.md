---
title:                "Gleam: htmlの解析"
simple_title:         "htmlの解析"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## なぜ
HTMLを解析することに関わるメリットは何か？
HTMLは情報を表現する簡単で柔軟な言語ですが、そのままでは機械が理解することができません。HTMLを解析することによって、機械が文書を読み込んでデータを取得することができるようになります。

## 方法
HTMLを解析するためには、GleamのHTMLパーサーライブラリを使用します。以下のようにコードを書くことで、HTMLを解析してデータを取得することができます。

```Gleam
import gleam/html/parser

html = """
<html>
    <head>
        <title>Gleam ブログ</title>
    </head>
    <body>
        <h1>こんにちは！</h1>
    </body>
</html>
"""

parsed_html = html |> parser.parse

title = parsed_html
    |> parser.find("head")
    |> parser.find("title")
    |> parser.text

header = parsed_html
    |> parser.find("h1")
    |> parser.text

IO.println(title) // => "Gleam ブログ"
IO.println(header) // => "こんにちは！"
```

## 詳細を深める
HTMLを解析するには、HTMLの構造を理解する必要があります。例えば、``<h1>``タグは見出しを表し、``<p>``タグは段落を表します。``find()``メソッドを使用することで、指定したタグを探し出し、その中のテキストを取得することができます。さらに、CSSセレクターを使用することで、特定の要素を取得することもできます。

## See Also
- GleamのHTMLパーサーライブラリについて：https://gleam.run/packages/gleam-lang/html-parser
- HTMLの基本：https://www.w3schools.com/html/default.asp
- CSSセレクターの使い方：https://www.w3schools.com/cssref/css_selectors.asp