---
title:                "テストの作成"
date:                  2024-02-03T19:30:44.748329-07:00
model:                 gpt-4-0125-preview
simple_title:         "テストの作成"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

Elixirでテストを書くことは、コードの振る舞いを検証する自動スクリプトを作成することを意味します。プログラマーはこれを行うことで品質を保証し、リグレッションを防ぎ、コードのリファクタリングを容易にして、開発プロセスをより信頼性が高く効率的なものにします。

## 方法：

Elixirは、非常に強力で使いやすい組み込みテストフレームワークとしてExUnitを使用します。基本的な例を挙げると以下の通りです：

1. Elixirプロジェクトの`test`ディレクトリに新しいテストファイルを作成します。たとえば、`MathOperations`というモジュールをテストしている場合、テストファイルは`test/math_operations_test.exs`となります。

```elixir
# test/math_operations_test.exs
defmodule MathOperationsTest do
  use ExUnit.Case

  # これは、加算関数をチェックするためのシンプルなテストケースです
  test "二つの数字の加算" do
    assert MathOperations.add(1, 2) == 3
  end
end
```

テストを実行するには、ターミナルで`mix test`コマンドを使用します。`MathOperations.add/2`関数が正しく2つの数字を加算すると、以下のような出力が表示されます：

```
..

0.03秒で終了
1テスト、0失敗
```

外部サービスやAPIを取り扱うテストの場合、実際のサービスを使用せずに済むよう`mox`などのモックライブラリを使用すると良いでしょう：

1. `mix.exs`の依存関係に`mox`を追加します：

```elixir
defp deps do
  [
    {:mox, "~> 1.0.0", only: :test},
    # 他の依存関係...
  ]
end
```

2. テストヘルパー（`test/test_helper.exs`）にモックモジュールを定義します：

```elixir
Mox.defmock(HTTPClientMock, for: HTTPClientBehaviour)
```

3. テストケースでモックを使用します：

```elixir
# test/some_api_client_test.exs
defmodule SomeAPIClientTest do
  use ExUnit.Case
  import Mox

  # この設定により、想定された通りこのモックが呼ばれたかどうかをMoxに検証させます
  setup :verify_on_exit!

  test "APIからデータを取得する" do
    # モックレスポンスを設定
    expect(HTTPClientMock, :get, fn _url -> {:ok, "モックされたレスポンス"} end)
    
    assert SomeAPIClient.get_data() == "モックされたレスポンス"
  end
end
```

`mix test`を実行する際、この設定により、ユニットテストを実際の外部依存性から分離して、自身のコードの振る舞いに集中できます。このパターンは、テストを迅速に実行し、外部サービスの状態やインターネット接続状況に関わらず、テストの信頼性を保証します。
