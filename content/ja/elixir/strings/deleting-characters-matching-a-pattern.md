---
date: 2024-01-20 17:41:57.063406-07:00
description: "\u6587\u5B57\u304C\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\
  \u3068\u3001\u305D\u308C\u3089\u3092\u524A\u9664\u3059\u308B\u3053\u3068\u304C\u3067\
  \u304D\u308B\u3002\u3053\u306E\u30D7\u30ED\u30BB\u30B9\u306F\u30C7\u30FC\u30BF\u3092\
  \u30AF\u30EA\u30FC\u30F3\u30A2\u30C3\u30D7\u3057\u305F\u308A\u3001\u4E0D\u8981\u306A\
  \u60C5\u5831\u3092\u53D6\u308A\u9664\u3044\u305F\u308A\u3059\u308B\u305F\u3081\u306B\
  \u3088\u304F\u4F7F\u308F\u308C\u308B\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3001\u30C7\u30FC\u30BF\u3092\u6574\u7406\u3057\u3066\u64CD\u4F5C\u3057\u3084\u3059\
  \u304F\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:39.743726-07:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u304C\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\
  \u3068\u3001\u305D\u308C\u3089\u3092\u524A\u9664\u3059\u308B\u3053\u3068\u304C\u3067\
  \u304D\u308B\u3002\u3053\u306E\u30D7\u30ED\u30BB\u30B9\u306F\u30C7\u30FC\u30BF\u3092\
  \u30AF\u30EA\u30FC\u30F3\u30A2\u30C3\u30D7\u3057\u305F\u308A\u3001\u4E0D\u8981\u306A\
  \u60C5\u5831\u3092\u53D6\u308A\u9664\u3044\u305F\u308A\u3059\u308B\u305F\u3081\u306B\
  \u3088\u304F\u4F7F\u308F\u308C\u308B\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3001\u30C7\u30FC\u30BF\u3092\u6574\u7406\u3057\u3066\u64CD\u4F5C\u3057\u3084\u3059\
  \u304F\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3046\u3002"
title: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\
  \u9664\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字がパターンに一致すると、それらを削除することができる。このプロセスはデータをクリーンアップしたり、不要な情報を取り除いたりするためによく使われる。プログラマーは、データを整理して操作しやすくするためにこれを行う。

## How to: (方法)
```Elixir
# 文字を削除するサンプルコード
defmodule PatternCleaner do
  def delete_matching_chars(string, pattern) do
    Regex.replace(~r/#{pattern}/, string, "")
  end
end

# 使用例
IO.puts PatternCleaner.delete_matching_chars("こんにちは123", "\\d")
```
出力:
```
こんにちは
```

## Deep Dive (掘り下げ)
Elixirでは、正規表現と`Regex`モジュールを使ってパターンに一致する文字を削除する。Elixirの前身であるErlangの時代から、パターンマッチングはプログラミングに欠かせない機能である。`String.replace/3`のような関数も同様の操作に使われるが、`Regex.replace/3`は複雑なパターンに柔軟に対応できる。実装の内部では、ElixirはErlangの正規表現ライブラリを利用しており、効率的な文字列操作が可能だ。

文字列の置換は、プログラミング言語によって異なる書き方がある。たとえば、Pythonでは`re.sub()`を、JavaScriptでは文字列の`replace()`メソッドを使う。Elixirは関数型言語であるため、不変性の原則に従って新しい文字列を返す形で操作を行う。

## See Also (関連情報)
- Elixirの正規表現ドキュメント: [https://hexdocs.pm/elixir/Regex.html](https://hexdocs.pm/elixir/Regex.html)
- `String`モジュールのドキュメント: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- Elixirフォーラムでの関連トピック: [https://elixirforum.com](https://elixirforum.com)
