---
title:                "HTMLの解析"
date:                  2024-01-20T15:31:18.851109-07:00
html_title:           "Arduino: HTMLの解析"
simple_title:         "HTMLの解析"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (何とその理由?)
HTMLのパースとは、HTMLドキュメントから情報を抽出するプロセスです。Webスクレイピングや自動化のため、プログラマーは構造化されたデータが必要で、そのために行います。

## How to: (やり方:)
ElixirにはHTMLをパースするためのライブラリがいくつかあります。ここでは`Floki`を使って基本的なパースを行う方法を見ていきましょう。まず、`Floki`をmix.exsに追加してインストールします。

```elixir
defp deps do
  [
    {:floki, "~> 0.30.0"}
  ]
end
```

そして、HTMLドキュメントをパースしてみましょう。

```elixir
html = """
<html>
  <body>
    <div class="content">
      <p>Hello, Elixir!</p>
      <p>Enjoy parsing.</p>
    </div>
  </body>
</html>
"""

{:ok, document} = Floki.parse_document(html)
paragraphs = Floki.find(document, ".content p")

IO.inspect paragraphs
```

出力は次のようになります。

```
[
  {"p", [], ["Hello, Elixir!"]},
  {"p", [], ["Enjoy parsing."]}
]
```

この例では、`.content`クラス内にある`<p>`タグのテキストを取得しました。

## Deep Dive (深掘り)
HTMLパースの必要性は1990年代初頭にさかのぼります。その頃、ウェブが急成長し、情報の自動取得が必要になりました。`Floki`のようなライブラリは、`HTML5ever`という耐久性のあるパースアルゴリズムを使い、Elixirでパイプライン操作をサポートします。そうすることで、HTMLのノード選択や属性の抽出が簡単になります。

別の選択肢としては、`MochiWeb`や`html5ever`のElixirバインディングがあります。これらは異なるアプローチや性能を提供することがありますが、`Floki`はその使いやすさと直感的なAPIで人気があります。

実際のHTMLのパースでは、ドキュメントがどのような構造をしているか、どの要素が必要かを熟知することが求められます。パフォーマンスも考慮しなければならず、巨大なHTMLドキュメントや複雑なセレクタでは効率が重要になります。

## See Also (関連情報)
- [Floki GitHub repository](https://github.com/philss/floki)
- [HexDocs for Floki](https://hexdocs.pm/floki/Floki.html)
- [Web scraping with Elixir](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html)
- [Selectorgadget – CSS Selector tool](http://selectorgadget.com/)
