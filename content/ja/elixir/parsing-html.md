---
title:                "HTMLの解析"
html_title:           "Elixir: HTMLの解析"
simple_title:         "HTMLの解析"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## なぜ

HTMLを解析することに取り組む *なぜ* ですか？

HTMLはウェブサイトを構築する際に使用される一般的な言語です。多くのウェブサイトがHTMLを使用するため、エンジニアやウェブ開発者にはHTMLを理解しておく必要があります。また、ウェブスクレイピングやデータ収集のためにHTMLを解析する必要があるかもしれません。

## 方法

まず、Elixirをお使いのコンピューターにインストールしてください。次に、Elixirプロジェクトを作成しましょう。

```Elixir
mkdir my_html_parser
cd my_html_parser
mix new my_html_parser
```

次に、HTTPリクエストを使ってHTMLを取得し、Nokogiriライブラリを使ってHTMLを解析します。

```Elixir
url = "https://www.example.com"
html = HTTPoison.get!(url).body
document = Floki.parse(html)

# ページのタイトルを取得する
title = Floki.find(document, "title") |> Floki.text

# リンクを取得する
links = Floki.find_all(document, "a") |> Floki.attribute("href")

# 画像を取得する
images = Floki.find_all(document, "img") |> Floki.attribute("src")
```

上記のコードでは、HTMLタグを指定して特定の情報を抽出することができます。Nokogiriライブラリは、HTMLをパースし、Flokiライブラリを使用することで、取得した情報をさまざまな形式で抽出することができます。

## ディープダイブ

HTMLを解析するには、HTMLの基本構造やタグの意味を理解する必要があります。さらに、HTML内のタグの階層や属性についても知る必要があります。また、さまざまなライブラリやフレームワークを使用してHTMLをパースする方法もあります。Elixirには、NokogiriやFlokiの他にもScrivenerやNimbleParsecなどの優れたライブラリがあります。

## 参考文献

- [Flokiライブラリ](https://github.com/philss/floki)
- [Nokogiriライブラリ](https://github.com/elixir-nokogiri/nokogiri)
- [Scrivenerライブラリ](https://github.com/awetzel/scrivener)
- [NimbleParsecライブラリ](https://github.com/dashbitco/nimble_parsec)