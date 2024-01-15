---
title:                "ウェブページのダウンロード"
html_title:           "Elixir: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## なぜ

ウェブページをダウンロードする理由は何でしょうか？ウェブスクレイピングやデータ収集のために、必要な情報を手軽に取得するためにダウンロードする方もいらっしゃるかもしれません。

## 方法

まず、Elixirをコンピュータにインストールします。次に、以下の例を参考に、ウェブページをダウンロードする方法を学びましょう。

```Elixir
require HTTPotion

page = HTTPotion.get("https://www.example.com")

puts page.body
```

上記のコードでは、HTTPotionライブラリを使用して、指定したURLからウェブページをダウンロードし、その内容を出力しています。ウェブページのHTMLやテキストデータを取得することができます。

## 深堀り

ウェブページをダウンロードする方法は様々ありますが、ElixirにはHTTPotionの他にもHTTPライブラリがあります。特に、HTTPoisonはより高度な機能を備えており、より多くの情報を取得することができます。また、Erlangの並列処理機能を使用することで、大量のウェブページを高速でダウンロードすることも可能です。

## 関連リンク

- [Elixir公式サイト](https://elixir-lang.org/)
- [HTTPotionドキュメント](https://hexdocs.pm/httpotion/readme.html)
- [HTTPoisonドキュメント](https://hexdocs.pm/httpoison/readme.html)