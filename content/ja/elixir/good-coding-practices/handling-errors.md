---
date: 2024-01-26 00:51:30.430784-07:00
description: "\u65B9\u6CD5 Elixir\u3067\u306F\u3001\u30D1\u30BF\u30FC\u30F3\u30DE\u30C3\
  \u30C1\u30F3\u30B0\u3068`case`\u30B9\u30C6\u30FC\u30C8\u30E1\u30F3\u30C8\u3092\u4F7F\
  \u7528\u3057\u3066\u3001\u30A8\u30E9\u30FC\u3092\u542B\u3080\u7570\u306A\u308B\u7D50\
  \u679C\u3092\u51E6\u7406\u3059\u308B\u3053\u3068\u304C\u3088\u304F\u3042\u308A\u307E\
  \u3059\u3002"
lastmod: '2024-04-05T21:53:42.567899-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u30A8\u30E9\u30FC\u51E6\u7406"
weight: 16
---

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
