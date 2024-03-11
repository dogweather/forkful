---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:00.721455-07:00
description: "Elixir \u3067\u306E HTML \u306E\u89E3\u6790\u306F\u3001HTML \u30C9\u30AD\
  \u30E5\u30E1\u30F3\u30C8\u304B\u3089\u60C5\u5831\u3092\u62BD\u51FA\u3059\u308B\u3053\
  \u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001\u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\u3001\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u3067\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u3068\u5BFE\u8A71\u3057\u305F\
  \u308A\u3001\u30C7\u30FC\u30BF\u3092\u30B9\u30AF\u30EC\u30A4\u30D4\u30F3\u30B0\u3057\
  \u305F\u308A\u3001\u30A6\u30A7\u30D6\u306E\u64CD\u4F5C\u3092\u81EA\u52D5\u5316\u3057\
  \u305F\u308A\u3067\u304D\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u304C\
  \u30A6\u30A7\u30D6\u30B3\u30F3\u30C6\u30F3\u30C4\u3092\u52D5\u7684\u306B\u7406\u89E3\
  \u3057\u3066\u5229\u7528\u3067\u304D\u308B\u3088\u3046\u306B\u3057\u307E\u3059\u3002"
lastmod: '2024-03-11T00:14:15.242942-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u3067\u306E HTML \u306E\u89E3\u6790\u306F\u3001HTML \u30C9\u30AD\
  \u30E5\u30E1\u30F3\u30C8\u304B\u3089\u60C5\u5831\u3092\u62BD\u51FA\u3059\u308B\u3053\
  \u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001\u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\u3001\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u3067\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u3068\u5BFE\u8A71\u3057\u305F\
  \u308A\u3001\u30C7\u30FC\u30BF\u3092\u30B9\u30AF\u30EC\u30A4\u30D4\u30F3\u30B0\u3057\
  \u305F\u308A\u3001\u30A6\u30A7\u30D6\u306E\u64CD\u4F5C\u3092\u81EA\u52D5\u5316\u3057\
  \u305F\u308A\u3067\u304D\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u304C\
  \u30A6\u30A7\u30D6\u30B3\u30F3\u30C6\u30F3\u30C4\u3092\u52D5\u7684\u306B\u7406\u89E3\
  \u3057\u3066\u5229\u7528\u3067\u304D\u308B\u3088\u3046\u306B\u3057\u307E\u3059\u3002"
title: "HTML\u306E\u89E3\u6790"
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
