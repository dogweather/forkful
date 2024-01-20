---
title:                "テストの作成"
html_title:           "Bash: テストの作成"
simple_title:         "テストの作成"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (テストの書き方とその理由)
テストはコードが期待通りに動くことを保証する仕組みです。バグの早期発見、機能の安全な改良、リファクタリングを容易にするためにプログラマーはテストを行います。

## How to (やり方):
Gleamでのテストは簡単です。以下はサンプルコード:

```gleam
import gleam/should
import my_module

pub fn test_my_function() {
  my_module.my_function()
  |> should.equal("expected result")
}

pub fn main() {
  should.run_test(test_my_function)
}
```

実行結果:

```
1 tests run, 0 failures
```

## Deep Dive (深掘り):
テストはソフトウェア開発において古くから実践されています。GleamにはErlangの豊富なテスティングフレームワークがバックグラウンドにありますが、Gleam固有のツールも開発されています。`gleam/expect`や`gleam/should`は使いやすく、関数の正確な挙動を確認するために作られました。代替の手法には手動テスティングやプロパティベースのテスティングもありますが、これらは時間がかかりエラーに繋がりやすいです。