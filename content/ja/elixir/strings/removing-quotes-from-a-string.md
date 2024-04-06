---
date: 2024-01-26 03:39:32.283867-07:00
description: "\u65B9\u6CD5\uFF1A Elixir\u306B\u306F\u7D44\u307F\u8FBC\u307F\u306E\u300C\
  \u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B\u300D\u6A5F\u80FD\u306F\u3042\u308A\
  \u307E\u305B\u3093\u304C\u3001\u30D1\u30BF\u30FC\u30F3\u30DE\u30C3\u30C1\u30F3\u30B0\
  \u3084`String`\u95A2\u6570\u3092\u7528\u3044\u3066\u81EA\u4F5C\u3059\u308B\u306E\
  \u306F\u7C21\u5358\u3067\u3059\u3002\u3053\u308C\u3089\u306E\u30B9\u30CB\u30DA\u30C3\
  \u30C8\u3092\u53C2\u7167\u3057\u3066\u304F\u3060\u3055\u3044\uFF1A."
lastmod: '2024-04-05T21:53:42.543739-06:00'
model: gpt-4-0125-preview
summary: ''
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
