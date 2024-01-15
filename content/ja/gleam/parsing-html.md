---
title:                "HTMLの解析"
html_title:           "Gleam: HTMLの解析"
simple_title:         "HTMLの解析"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## なぜ

HTMLをパースすることが重要かつ便利な理由をご紹介します。

パースとは、データを解析することを指します。HTMLはウェブサイトを構築する上で欠かせない言語であり、そのデータを解析することで、より効率的にウェブページを作成することができます。また、ウェブスクレイピングやデータ収集などの用途にも利用することができます。

## 方法

以下のようなGleamのコード例を使いながら、HTMLのパースを実践的に学びましょう。

```Gleam
// 必要なライブラリをインポート
import gleam/html
import gleam/json
import gleam/http

// ウェブサイトのURLを指定
let url = "https://example.com"

// ページのデータを取得
let html = http.get(url).body

// パースしたい要素の指定
let selector = "h1"

// HTMLのパースを実行
let result = html |> html.parse |> html.select(selector)

// 結果をJSON形式で文字列に変換
let json = result |> json.from(html_to_show)

// パースした要素を表示
pub fn show() {
  json_to_show |> json.encode |> std_io.format |> std_io.print
}
```

上記のコードでは、Gleamのライブラリを使用してウェブサイトからHTMLデータを取得し、指定した要素を抽出し、最終的にJSON形式で表示することができます。

## ディープダイブ

HTMLのパースには、いくつかの方法があります。上記のコードでは、`html.select`メソッドを使用しましたが、他にも`html.parse`や`html.decode`などのメソッドを使用することで、より詳細なパースが可能です。また、正規表現を使うことで、さらに柔軟なパースが可能です。

さらに、Gleamではパターンマッチングを用いた強力なデータ構造が利用できるため、取得したHTMLデータを自由自在に操作することができます。これにより、より高度なウェブスクレイピングやデータ収集を実現することができます。

## 参考リンク

- [Gleam公式ドキュメント](https://gleam.run/getting-started/)
- [GleamのHTMLライブラリのドキュメント](https://gleam.run/packages/html/)
- [Regular Expressions in Gleam](https://shinseitan.net/gleam-regular-expressions/)