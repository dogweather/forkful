---
date: 2024-01-26 00:51:30.430784-07:00
description: "\u30A8\u30E9\u30FC\u3092\u6271\u3046\u3068\u306F\u3001\u7269\u4E8B\u304C\
  \u4E0A\u624B\u304F\u3044\u304B\u306A\u3044\u5834\u5408\u306B\u5BFE\u51E6\u3067\u304D\
  \u308B\u30B3\u30FC\u30C9\u3092\u66F8\u304F\u3053\u3068\u3092\u610F\u5473\u3057\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u305D\u308C\u3092\u884C\u3044\
  \u3001\u30AF\u30E9\u30C3\u30B7\u30E5\u3092\u9632\u304E\u3001\u30DE\u30FC\u30D5\u30A3\
  \u30FC\u306E\u6CD5\u5247\u304C\u767A\u52D5\u3057\u305F\u6642\u306B\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u304C\u512A\u96C5\u306B\u56DE\u5FA9\u3067\u304D\u308B\u3088\u3046\u306B\
  \u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.621602-06:00'
model: gpt-4-1106-preview
summary: "\u30A8\u30E9\u30FC\u3092\u6271\u3046\u3068\u306F\u3001\u7269\u4E8B\u304C\
  \u4E0A\u624B\u304F\u3044\u304B\u306A\u3044\u5834\u5408\u306B\u5BFE\u51E6\u3067\u304D\
  \u308B\u30B3\u30FC\u30C9\u3092\u66F8\u304F\u3053\u3068\u3092\u610F\u5473\u3057\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u305D\u308C\u3092\u884C\u3044\
  \u3001\u30AF\u30E9\u30C3\u30B7\u30E5\u3092\u9632\u304E\u3001\u30DE\u30FC\u30D5\u30A3\
  \u30FC\u306E\u6CD5\u5247\u304C\u767A\u52D5\u3057\u305F\u6642\u306B\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u304C\u512A\u96C5\u306B\u56DE\u5FA9\u3067\u304D\u308B\u3088\u3046\u306B\
  \u3057\u307E\u3059\u3002."
title: "\u30A8\u30E9\u30FC\u51E6\u7406"
weight: 16
---

## 何となく理由

エラーを扱うとは、物事が上手くいかない場合に対処できるコードを書くことを意味します。プログラマーはそれを行い、クラッシュを防ぎ、マーフィーの法則が発動した時にプログラムが優雅に回復できるようにします。

## 方法

Elixirでは、パターンマッチングと`case`ステートメントを使用して、エラーを含む異なる結果を処理することがよくあります。

```elixir
defmodule Example do
  def divide(a, b) do
    case b do
      0 -> {:error, "ゼロで割ることはできません。"}
      _ -> {:ok, a / b}
    end
  end
end

# 成功した除算
{:ok, result} = Example.divide(10, 2)
IO.puts("10 / 2 は #{result}")

# ゼロで割る試み
{:error, reason} = Example.divide(10, 0)
IO.puts("エラー: #{reason}")
```

サンプル出力：
```
10 / 2 は 5.0
エラー: ゼロで割ることはできません。
```

このElixirコードを実行すると、入力に応じて成功した除算またはエラーメッセージのいずれかが得られます。ここではクラッシュはありません！

## 深掘り

昔はエラー処理はしばしば戻り値のチェックに関して行われました。しかしElixirの関数型のルーツのおかげで、`{:ok, value}`や`{:error, reason}`のようなタグ付けされたタプルとパターンマッチングがあり、それらはよりエレガントです。

Elixirでのエラー処理のその他の方法：

- **Elixirの`try`と`rescue`**は命令型言語の伝統的な`try-catch`に似ていますが、Elixirの明示性を好むため、頻繁には使用されません。
- **スーパーバイザーとGenServers**は、ElixirのOTPフレームワークの一部であり、障害耐性に関するものです。それらはあなたのコードのプロセスを監視し、物事がおかしくなったら再起動する準備ができています。

実装の面では、ElixirはErlangの堅牢性に基づいて構築されています。エラーはパターンマッチングと関数型の良さを使って扱うべきもう一種のメッセージとして扱われます。

## 参照する

Elixirでのエラー処理に関するさらなる読み物：

- 公式のガイド[Elixirのエラー処理](https://elixir-lang.org/getting-started/try-catch-and-rescue.html)。
- [プロセスとOTPについての詳細](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html)を学ぶ。
- Elixir Forumは質問するのにいつもいい場所です: [https://elixirforum.com](https://elixirforum.com)。
