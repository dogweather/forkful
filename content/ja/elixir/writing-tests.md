---
title:                "テストの作成"
html_title:           "Bash: テストの作成"
simple_title:         "テストの作成"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ?)
テストコードとは、プログラムが意図した通りに動作することを保証するためのコードです。バグを早期に発見し、安心して機能を追加・改良できるようにするためにプログラマはテストを書きます。

## How to: (やり方)
Elixirでテストを書く基礎を紹介します。ExUnitが標準のテストフレームワークです。

```elixir
# test/example_test.exs
defmodule ExampleTest do
  use ExUnit.Case
  doctest YourModule

  test "the truth" do
    assert 1 + 1 == 2
  end
end
```

テストを実行するには、以下のコマンドを使います。

```shell
$ mix test
```

期待する出力は以下の通りです。

```
..

Finished in 0.04 seconds
2 tests, 0 failures
```

## Deep Dive (深堀り)
テストはXP(エクストリーム・プログラミング)の出現と共に普及しました。Elixirでは、ExUnitを使うことが一般的ですが、ErlangのCommon Testなどの代替品もあります。ExUnitはsetup/bare_testなど、細かいテストの設定やコンテキストの管理が可能です。

## See Also (関連情報)
- [Elixir の公式ドキュメント（ExUnitについて）](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Programming Elixir（書籍）](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)
- [Elixir School（テストレッスン）](https://elixirschool.com/en/lessons/basics/testing/)
