---
title:                "エラー処理"
aliases: - /ja/elixir/handling-errors.md
date:                  2024-01-26T00:51:30.430784-07:00
model:                 gpt-4-1106-preview
simple_title:         "エラー処理"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/handling-errors.md"
---

{{< edit_this_page >}}

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
