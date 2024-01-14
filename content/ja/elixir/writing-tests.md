---
title:    "Elixir: テストの作成"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/writing-tests.md"
---

{{< edit_this_page >}}

# なぜテストを作成するのか

プログラミングにおいて、テストは非常に重要な役割を果たします。テストを作成することで、コードの品質を維持し、バグを見つけて修正することができます。こうすることで、プログラムが期待通りに動作することを確認することができます。

# テストの作り方

テストを作成するためには、Elixirに組み込まれているExUnitフレームワークを使用することができます。まずは、テストファイルを作成しましょう。例えば、"calculator_test.exs"というファイル名で作成します。

```Elixir
defmodule CalculatorTest do 
  use ExUnit.Case

  test "addition test" do 
    assert Calculator.add(2, 3) == 5
  end
end
```

このように、テストケースを作成し、期待する結果が得られるかどうかをassert関数を使って確認します。その後、コマンドラインで"mix test"を実行すると、テストが実行されます。

```bash
$ mix test

.....

Finished in 0.05 seconds
5 tests, 0 failures
```

テストが成功した場合は、"."が表示され、失敗した場合は"F"が表示されます。全てのテストが成功すると、緑色の"Finished"が表示されます。

# 深堀り

実際のプロジェクトでは、様々な種類のテストを作成することができます。UnitテストやIntegrationテストなどがあり、それぞれのテストが役割を果たしています。また、個々のテストケースの実行順序を制御することもできます。詳細については、公式ドキュメントや参考リンクを参照してください。

## 参考リンク

- [ExUnit Documentation](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Elixir School - Testing](https://elixirschool.com/ja/lessons/basics/testing/)

# 関連リンク

- [Elixir 公式サイト](https://elixir-lang.org/)
- [Elixir 公式ドキュメント](https://elixir-lang.org/docs.html)
- [Elixir 公式フォーラム (英語)](https://elixirforum.com/)