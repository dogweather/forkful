---
title:                "HTMLの解析"
aliases:
- /ja/elixir/parsing-html.md
date:                  2024-02-03T19:12:00.721455-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTMLの解析"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

Elixir での HTML の解析は、HTML ドキュメントから情報を抽出することを意味します。プログラマーは、これを行うことで、プログラムでウェブページと対話したり、データをスクレイピングしたり、ウェブの操作を自動化したりでき、アプリケーションがウェブコンテンツを動的に理解して利用できるようにします。

## 方法：

Elixir は、堅牢な並行性モデルおよび関数型プログラミングパラダイムを持っていますが、組み込みの HTML 解析機能は含まれていません。しかし、この目的のために `Floki` のような人気のあるサードパーティ製ライブラリを使用できます。Floki は、Elixir のパターンマッチングとパイピング機能を活用して、HTML 解析を直感的かつ効率的に行えるようにします。

まず、mix.exs の依存関係に Floki を追加します:

```elixir
defp deps do
  [
    {:floki, "~> 0.31.0"}
  ]
end
```

次に、`mix deps.get` を実行して新しい依存関係をインストールします。

さて、簡単な HTML 文字列を解析してデータを抽出してみましょう。`<h1>` タグの中のタイトルを探します:

```elixir
html_content = """
<html>
  <body>
    <h1>Hello, Elixir!</h1>
    <h1>Another Title</h1>
  </body>
</html>
"""

titles = html_content
         |> Floki.find("h1")
         |> Floki.text()

IO.inspect(titles)
```

**サンプル出力:**

```elixir
["Hello, Elixir!", "Another Title"]
```

さらに深く掘り下げると、リンク (`<a>` タグ) とその href 属性を抽出したいとしましょう。これを達成する方法はこちらです:

```elixir
html_content = """
<html>
  <body>
    <a href="https://elixir-lang.org/">Elixir の公式ウェブサイト</a>
    <a href="https://hexdocs.pm/">HexDocs</a>
  </body>
</html>
"""

links = html_content
        |> Floki.find("a")
        |> Enum.map(fn({_, attrs, [text]}) -> {text, List.keyfind(attrs, "href", 0)} end)
        
IO.inspect(links)
```

**サンプル出力:**

```elixir
[{"Elixir の公式ウェブサイト", {"href", "https://elixir-lang.org/"}}, {"HexDocs", {"href", "https://hexdocs.pm/"}}]
```

このアプローチを用いることで、Elixir アプリケーションでウェブデータの抽出や操作タスクを効率的に行い、HTML ドキュメントをナビゲートして解析することができます。
