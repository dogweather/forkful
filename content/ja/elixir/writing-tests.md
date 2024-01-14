---
title:    "Elixir: テストの書き方"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## なぜテストを書くのか

プログラムを書く上で、バグやエラーを防ぐためにテストを書くことが重要です。テストを書くことで、コードに潜む問題を早期に発見し修正することができます。テストはプログラムの品質を向上させ、安定性を保つために必要不可欠です。

## テストの書き方

Elixirでは、テストフレームワークとしてExUnitが提供されています。まずはExUnitをインポートし、テストを書くための関数である`defmodule`と`test`を使用します。以下のコードは、簡単な足し算のテストを行う例です。

```elixir
defmodule MathTest do 
  use ExUnit.Case 
  
  test "addition" do 
    assert Math.add(1, 2) == 3 
  end 
end
```

ExUnitでは、`assert`という関数を使用して、テストの結果を確認します。もしテストに合格した場合は何も表示されませんが、合格しなかった場合はエラーが表示されます。また、テストの関数名は明確なものにすることが重要です。上記の例では、「addition」というテストを実行しているため、後でコードを確認する際にどのテストを実行していたのかを把握しやすくなります。

## 深堀り

テストを書く際に重要なのは、可能な限り網羅的なテストを行うことです。これは、各関数やモジュールに対して、異なる入力や状況を想定してテストを行うことを意味します。また、リファクタリングを行った際にもテストを行うことで、予期せぬ結果が発生しないようにすることも重要です。

さらに、Elixirではプロパティベーステストを行うこともできます。これは、プロパティや条件を満たす複数の値を自動的に入力し、アサーションを行うものです。これにより、より網羅的なテストを行うことができ、コードの品質を向上させることができます。

## See Also

- [ExUnit Documentation](https://hexdocs.pm/ex_unit/ExUnit.html)
- [The Elixir Test-Driven Development Series](https://blog.carbonfive.com/elixir-test-driven-development-series/)