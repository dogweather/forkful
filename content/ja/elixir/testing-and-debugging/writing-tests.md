---
aliases:
- /ja/elixir/writing-tests/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:44.748329-07:00
description: "Elixir\u3067\u30C6\u30B9\u30C8\u3092\u66F8\u304F\u3053\u3068\u306F\u3001\
  \u30B3\u30FC\u30C9\u306E\u632F\u308B\u821E\u3044\u3092\u691C\u8A3C\u3059\u308B\u81EA\
  \u52D5\u30B9\u30AF\u30EA\u30D7\u30C8\u3092\u4F5C\u6210\u3059\u308B\u3053\u3068\u3092\
  \u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\
  \u308C\u3092\u884C\u3046\u3053\u3068\u3067\u54C1\u8CEA\u3092\u4FDD\u8A3C\u3057\u3001\
  \u30EA\u30B0\u30EC\u30C3\u30B7\u30E7\u30F3\u3092\u9632\u304E\u3001\u30B3\u30FC\u30C9\
  \u306E\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3092\u5BB9\u6613\u306B\u3057\
  \u3066\u3001\u958B\u767A\u30D7\u30ED\u30BB\u30B9\u3092\u3088\u308A\u4FE1\u983C\u6027\
  \u304C\u9AD8\u304F\u52B9\u7387\u7684\u306A\u3082\u306E\u306B\u3057\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:54.646427
model: gpt-4-0125-preview
summary: "Elixir\u3067\u30C6\u30B9\u30C8\u3092\u66F8\u304F\u3053\u3068\u306F\u3001\
  \u30B3\u30FC\u30C9\u306E\u632F\u308B\u821E\u3044\u3092\u691C\u8A3C\u3059\u308B\u81EA\
  \u52D5\u30B9\u30AF\u30EA\u30D7\u30C8\u3092\u4F5C\u6210\u3059\u308B\u3053\u3068\u3092\
  \u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\
  \u308C\u3092\u884C\u3046\u3053\u3068\u3067\u54C1\u8CEA\u3092\u4FDD\u8A3C\u3057\u3001\
  \u30EA\u30B0\u30EC\u30C3\u30B7\u30E7\u30F3\u3092\u9632\u304E\u3001\u30B3\u30FC\u30C9\
  \u306E\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3092\u5BB9\u6613\u306B\u3057\
  \u3066\u3001\u958B\u767A\u30D7\u30ED\u30BB\u30B9\u3092\u3088\u308A\u4FE1\u983C\u6027\
  \u304C\u9AD8\u304F\u52B9\u7387\u7684\u306A\u3082\u306E\u306B\u3057\u307E\u3059\u3002"
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
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
