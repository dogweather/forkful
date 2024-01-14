---
title:                "Elixir: HTMLの解析"
simple_title:         "HTMLの解析"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## なぜ
人々がHTMLパースを行う理由を説明するための1-2文。

HTMLをパースするには、ウェブページから情報を収集する必要があるかもしれません。または、特定のパターンを検索して情報を抽出したい場合もあります。ElixirのHTMLパーサーは、このようなタスクを非常に簡単に実行できます。

## 使い方
以下の「```Elixir ... ```」コードブロックには、どのようにHTMLパースを実行するかの例と出力が含まれています。

```Elixir
# HTMLパーサーを使って特定の要素をパースする
html = "<div><h1>Hello World!</h1></div>"
parsed = HTML.parse(html)
IO.inspect parsed
# 出力: [{:div, [], [{:h1, [], ["Hello World!"]}]}]

# パータン検索を実行して情報を抽出する
html = "<div><h1>Hello World!</h1><p>This is a paragraph</p></div>"
parsed = HTML.parse(html, "div > p")
IO.inspect parsed
# 出力: ["This is a paragraph"]
```

## ディープダイブ
HTMLパーサーには、さまざまなオプションがあります。例えば、特定の要素の属性やテキストを抽出したり、抽出されたデータをリストやマップに変換したりすることができます。詳細については、公式ドキュメントを参照してください。

## 参考
こちらのリンクを参考にしてください。

- [Elixir HTMLパーサー公式ドキュメント](https://hexdocs.pm/html/HTML.html)
- [Elixirウェブスクレイピングチュートリアル](https://dev.to/mnishiguchi/how-to-scrape-websites-with-elixir-and-exactor-4cdo)
 これを読むことで、より実践的なHTMLパースを行うことができます。