---
date: 2024-01-20 17:41:57.063406-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:37:49.923351-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Elixir\u3067\u306F\u3001\u6B63\u898F\u8868\u73FE\u3068`Regex`\u30E2\
  \u30B8\u30E5\u30FC\u30EB\u3092\u4F7F\u3063\u3066\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\
  \u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\u9664\u3059\u308B\u3002Elixir\u306E\u524D\
  \u8EAB\u3067\u3042\u308BErlang\u306E\u6642\u4EE3\u304B\u3089\u3001\u30D1\u30BF\u30FC\
  \u30F3\u30DE\u30C3\u30C1\u30F3\u30B0\u306F\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\
  \u306B\u6B20\u304B\u305B\u306A\u3044\u6A5F\u80FD\u3067\u3042\u308B\u3002`String.replace/3`\u306E\
  \u3088\u3046\u306A\u95A2\u6570\u3082\u540C\u69D8\u306E\u64CD\u4F5C\u306B\u4F7F\u308F\
  \u308C\u308B\u304C\u3001`Regex.replace/3`\u306F\u8907\u96D1\u306A\u30D1\u30BF\u30FC\
  \u30F3\u306B\u67D4\u8EDF\u306B\u5BFE\u5FDC\u3067\u304D\u308B\u3002\u5B9F\u88C5\u306E\
  \u5185\u90E8\u3067\u306F\u3001Elixir\u306FErlang\u306E\u6B63\u898F\u8868\u73FE\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u3092\u5229\u7528\u3057\u3066\u304A\u308A\u3001\u52B9\u7387\
  \u7684\u306A\u6587\u5B57\u5217\u64CD\u4F5C\u304C\u53EF\u80FD\u3060\u3002"
title: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\
  \u9664\u3059\u308B"
weight: 5
---

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
