---
title:                "Elixir: テストの書き方"
simple_title:         "テストの書き方"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/writing-tests.md"
---

{{< edit_this_page >}}

Elixirでテストを書くことの重要性

## なぜテストを書くのか

プログラマーにとって、バグのない安定したコードを書くことは非常に重要です。テストを書くことによって、バグを早期に発見し、コードの信頼性を高めることができます。また、新しい機能を追加した際に既存のコードが壊れていないかを確認することもできます。

## テストの書き方

テストは `ExUnit` フレームワークを使って書きます。書き方はとても簡単です。まずはテストを実行したいモジュールをインポートします。

```Elixir
# test/test_module.exs
defmodule TestModuleTest do
  use ExUnit.Case

  import TestModule
end
```

次に、`ExUnit` のマクロを使ってテストケースを定義します。

```Elixir
# test/test_module.exs
defmodule TestModuleTest do
  use ExUnit.Case

  import TestModule

  # テストケース名は "test_" で始める必要があります
  test "test_function/2 が適切な結果を返すこと" do
    assert test_function(2, 3) == 5
  end
end
```

最後に、コンソールから `mix test` コマンドを実行することで、テストを実行することができます。

## テストの詳細

テストを書く際には、コードカバレッジを意識することも重要です。コードカバレッジとは、テストがどれだけコードを網羅しているかを表す指標です。また、ユニットテストだけでなく、統合テストも書くことで、システム全体の信頼性をより高めることができます。

## 関連リンク

- [ExUnit ドキュメント](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Elixir チュートリアル](https://elixir-lang.org/getting-started/introduction.html)
- [Elixir School](https://elixirschool.com/ja/)