---
date: 2024-01-26 04:39:32.350436-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Elixir\u306B\u306F\u7D44\
  \u307F\u8FBC\u307F\u306E\u8907\u7D20\u6570\u304C\u306A\u3044\u305F\u3081\u3001\u81EA\
  \u5206\u3067\u4F5C\u6210\u3059\u308B\u304B\u3001`ComplexNum`\u306E\u3088\u3046\u306A\
  \u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\u3053\u3053\
  \u3067\u306F\u3001\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3057\u305F\u7C21\
  \u5358\u306A\u4F8B\u3092\u7D39\u4ECB\u3057\u307E\u3059\uFF1A."
lastmod: '2024-04-05T22:37:49.936152-06:00'
model: gpt-4-0125-preview
summary: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Elixir\u306B\u306F\u7D44\
  \u307F\u8FBC\u307F\u306E\u8907\u7D20\u6570\u304C\u306A\u3044\u305F\u3081\u3001\u81EA\
  \u5206\u3067\u4F5C\u6210\u3059\u308B\u304B\u3001`ComplexNum`\u306E\u3088\u3046\u306A\
  \u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\u3053\u3053\
  \u3067\u306F\u3001\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3057\u305F\u7C21\
  \u5358\u306A\u4F8B\u3092\u7D39\u4ECB\u3057\u307E\u3059\uFF1A."
title: "\u8907\u7D20\u6570\u306E\u6271\u3044\u65B9"
weight: 14
---

## どのようにして：
Elixirには組み込みの複素数がないため、自分で作成するか、`ComplexNum`のようなライブラリを使用します。ここでは、ライブラリを使用した簡単な例を紹介します：

```elixir
# ComplexNumがインストールされていると仮定
defmodule ComplexMath do
  import ComplexNum

  def add(a, b) do
    ComplexNum.add(a, b)
  end
end

# 複素数を作成してそれらを加算する
c1 = {3, 4}   # 3 + 4iを表す
c2 = {2, -3}  # 2 - 3iを表す
result = ComplexMath.add(c1, c2)
IO.puts "結果は：#{inspect(result)}"
```

この出力になります：
```
結果は：{5, 1}
```

これは、`3 + 4i`と`2 - 3i`の合計が`5 + 1i`であることを意味します。

## 深掘り
複素数は、マイナスの平方根を通常の数字では扱えなかったため、歴史の中で現れました。それらが真剣に受け止められたのは17世紀までなく、レネ・デカルトやジェロラモ・カルダノのような数学者のおかげでした。

Elixirでは、複素数には`{3, 4}`のようなタプルをよく使いますが、車輪の再発明を避けるために専用のライブを使用します。通常、ライブラリの方が優れています。これは、虚数単位'i'（FYI：`i`の二乗は`-1`に等しい）のためにトリッキーになる乗算や除算など、細かい点を処理します。

## 関連情報
これらのリソースもチェックしてみてください：
- ElixirのパッケージマネージャHex用の[ComplexNumライブラリ](https://hex.pm/packages/complex_num)。
- 高度なElixirのトピックや演習については、[Elixir School](https://elixirschool.com/en/)。
- Elixirが内部で使用するErlangの[mathモジュール](http://erlang.org/doc/man/math.html)、その他の数学的なニーズについて。
