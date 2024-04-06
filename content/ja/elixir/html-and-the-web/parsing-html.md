---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:00.721455-07:00
description: "\u65B9\u6CD5\uFF1A Elixir \u306F\u3001\u5805\u7262\u306A\u4E26\u884C\
  \u6027\u30E2\u30C7\u30EB\u304A\u3088\u3073\u95A2\u6570\u578B\u30D7\u30ED\u30B0\u30E9\
  \u30DF\u30F3\u30B0\u30D1\u30E9\u30C0\u30A4\u30E0\u3092\u6301\u3063\u3066\u3044\u307E\
  \u3059\u304C\u3001\u7D44\u307F\u8FBC\u307F\u306E HTML \u89E3\u6790\u6A5F\u80FD\u306F\
  \u542B\u307E\u308C\u3066\u3044\u307E\u305B\u3093\u3002\u3057\u304B\u3057\u3001\u3053\
  \u306E\u76EE\u7684\u306E\u305F\u3081\u306B `Floki` \u306E\u3088\u3046\u306A\u4EBA\
  \u6C17\u306E\u3042\u308B\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u88FD\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3067\u304D\u307E\u3059\u3002Floki \u306F\u3001\
  Elixir\u2026"
lastmod: '2024-04-05T22:37:49.940822-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Elixir \u306F\u3001\u5805\u7262\u306A\u4E26\u884C\u6027\
  \u30E2\u30C7\u30EB\u304A\u3088\u3073\u95A2\u6570\u578B\u30D7\u30ED\u30B0\u30E9\u30DF\
  \u30F3\u30B0\u30D1\u30E9\u30C0\u30A4\u30E0\u3092\u6301\u3063\u3066\u3044\u307E\u3059\
  \u304C\u3001\u7D44\u307F\u8FBC\u307F\u306E HTML \u89E3\u6790\u6A5F\u80FD\u306F\u542B\
  \u307E\u308C\u3066\u3044\u307E\u305B\u3093\u3002\u3057\u304B\u3057\u3001\u3053\u306E\
  \u76EE\u7684\u306E\u305F\u3081\u306B `Floki` \u306E\u3088\u3046\u306A\u4EBA\u6C17\
  \u306E\u3042\u308B\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u88FD\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u3092\u4F7F\u7528\u3067\u304D\u307E\u3059\u3002Floki \u306F\u3001Elixir\
  \ \u306E\u30D1\u30BF\u30FC\u30F3\u30DE\u30C3\u30C1\u30F3\u30B0\u3068\u30D1\u30A4\
  \u30D4\u30F3\u30B0\u6A5F\u80FD\u3092\u6D3B\u7528\u3057\u3066\u3001HTML \u89E3\u6790\
  \u3092\u76F4\u611F\u7684\u304B\u3064\u52B9\u7387\u7684\u306B\u884C\u3048\u308B\u3088\
  \u3046\u306B\u3057\u307E\u3059\u3002 \u307E\u305A\u3001mix.exs \u306E\u4F9D\u5B58\
  \u95A2\u4FC2\u306B Floki \u3092\u8FFD\u52A0\u3057\u307E\u3059."
title: "HTML\u306E\u89E3\u6790"
weight: 43
---

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
