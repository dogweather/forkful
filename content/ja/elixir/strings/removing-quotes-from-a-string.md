---
date: 2024-01-26 03:39:32.283867-07:00
description: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u5265\u304C\u3059\
  \u3068\u3044\u3046\u306E\u306F\u3001\u305D\u306E\u4F59\u5206\u306A\u30E9\u30C3\u30D1\
  \u30FC\u3092\u53D6\u308A\u9664\u3044\u3066\u4E2D\u306E\u30AF\u30EA\u30FC\u30F3\u30C8\
  \u30C6\u30AD\u30B9\u30C8\u3092\u5F97\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u3053\u308C\u3092\u884C\u3046\
  \u306E\u306F\u3001\u5165\u529B\u3092\u30B5\u30CB\u30BF\u30A4\u30BA\u3057\u305F\u308A\
  \u3001\u30A8\u30E9\u30FC\u3092\u907F\u3051\u305F\u308A\u3001\u5F15\u7528\u7B26\u304C\
  \u6A5F\u80FD\u3067\u306F\u306A\u304F\u969C\u5BB3\u3068\u306A\u308B\u51E6\u7406\u306E\
  \u305F\u3081\u306B\u30C7\u30FC\u30BF\u3092\u6E96\u5099\u3059\u308B\u305F\u3081\u3067\
  \u3059\u3002"
lastmod: '2024-03-13T22:44:41.596944-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u5265\u304C\u3059\
  \u3068\u3044\u3046\u306E\u306F\u3001\u305D\u306E\u4F59\u5206\u306A\u30E9\u30C3\u30D1\
  \u30FC\u3092\u53D6\u308A\u9664\u3044\u3066\u4E2D\u306E\u30AF\u30EA\u30FC\u30F3\u30C8\
  \u30C6\u30AD\u30B9\u30C8\u3092\u5F97\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u3053\u308C\u3092\u884C\u3046\
  \u306E\u306F\u3001\u5165\u529B\u3092\u30B5\u30CB\u30BF\u30A4\u30BA\u3057\u305F\u308A\
  \u3001\u30A8\u30E9\u30FC\u3092\u907F\u3051\u305F\u308A\u3001\u5F15\u7528\u7B26\u304C\
  \u6A5F\u80FD\u3067\u306F\u306A\u304F\u969C\u5BB3\u3068\u306A\u308B\u51E6\u7406\u306E\
  \u305F\u3081\u306B\u30C7\u30FC\u30BF\u3092\u6E96\u5099\u3059\u308B\u305F\u3081\u3067\
  \u3059\u3002."
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
weight: 9
---

## 方法：
Elixirには組み込みの「引用符を削除する」機能はありませんが、パターンマッチングや`String`関数を用いて自作するのは簡単です。これらのスニペットを参照してください：

```elixir
# パターンマッチングを使用
def unquote_string("\"" <> quoted_string <> "\""), do: quoted_string
def unquote_string("'" <> quoted_string <> "'"), do: quoted_string
def unquote_string(quoted_string), do: quoted_string

# サンプル使用法
unquote_string("\"Hello, World!\"") # => "Hello, World!"
unquote_string("'Hello, World!'")   # => "Hello, World!"

# String.trim/1を使用
def unquote_string(string), do: String.trim(string, "'\"")

# サンプル使用法
unquote_string("\"Hello, World!\"") # => "Hello, World!"
unquote_string("'Hello, World!'")   # => "Hello, World!"
```

どちらの方法でも出力は以下になります：
```
"Hello, World!"
```

## ディープダイブ
昔、文字列の中の引用符は地雷原でした—扱いを誤れば、爆発的に構文エラーやセキュリティホールが生じてしまいます。Elixirでは、パターンマッチングが文字列をレゴブロックのように扱い、精度高く分解して再構築させます。その頑丈な`String`モジュールも、`trim`関数で引用符を柔軟に取り除くのに便利です。代替手段としては、正規表現が引用符を退治したり、基本的な剥がし以上の機能が必要な場合は外部ライブラリが追加の火力を提供するかもしれません。

## 参照
これらで深く掘り下げてみてください：
- [ElixirのStringモジュール](https://hexdocs.pm/elixir/String.html)
- [Elixirにおけるパターンマッチングについてもっと学ぶ](https://elixir-lang.org/getting-started/pattern-matching.html)
- [Elixirでの正規表現（Regexモジュール）](https://hexdocs.pm/elixir/Regex.html)
