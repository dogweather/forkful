---
title:                "HTML パース"
html_title:           "Gleam: HTML パース"
simple_title:         "HTML パース"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## 何となぜ？
パーシングHTMLとは、HTMLを分析して必要なデータを取り出すことです。プログラマーがこれを行うのは、例えばWebサイトから情報を収集したり、自動化したりするためです。

## 方法：
```Gleam ... ```のコードブロック内に、コーディング例とサンプルの出力を示します。

**例1：HTMLから特定のテキストを抽出する**

```Gleam 
let input_html = "
<html>
<head>
<title>Gleam Parsing Article</title>
</head>
<body>
<h1>This is a title</h1>
<p>This is a paragraph</p>
</body>
</html>
"

let parsed = Html.parse(input_html)
let h1 = Html.get_element(parsed, "h1")
let p = Html.get_element(parsed, "p")

Debug.assert_equal("This is a title", Html.get_text(h1))
Debug.assert_equal("This is a paragraph", Html.get_text(p)) 
```

**例2：HTMLをフォーマットする**

```Gleam
let input_html = "
<html>
<body>
<h1>This is a title</h1>
</body>
</html>
"

let formatted = Html.format(input_html)
Debug.assert_equal(
  "<html>\n<body>\n  <h1>This is a title</h1>\n</body>\n</html>",
  formatted
)
```

## 深く掘り下げる：
パーシングHTMLは、Web開発の歴史や現在のWebスクレイピングやデータ収集の手法の一つです。他にも、パーシングHTMLの代替手法としてXMLパーサーや正規表現があります。また、GleamでのHTMLパーシングは、一般的なHTMLパーサーよりもシンプルで柔軟な実装が特徴です。

## 関連情報：
- [Gleam公式ドキュメント](https://gleam.run/)
- [HTMLパーサーの一覧](https://github.com/zbjornson/awesome-html-parsing)
- [正規表現を使ったテキスト抽出の例](https://regexr.com/)