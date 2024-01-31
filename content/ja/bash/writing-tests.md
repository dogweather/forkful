---
title:                "テストの作成"
date:                  2024-01-19
html_title:           "Bash: テストの作成"
simple_title:         "テストの作成"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
プログラムにテストを書くことは、コードが期待通りに動作することを保証するプロセスです。これにより、バグを早期に見つけ、将来的な機能追加やリファクタリング時の安心を得られます。

## How to: (やり方)
```Bash
# test_example.sh

# 関数定義
say_hello() {
  echo "Hello, $1!"
}

# テストケース
test_say_hello() {
  output=$(say_hello "World")
  if [ "$output" == "Hello, World!" ]; then
    echo "Test passed!"
  else
    echo "Test failed: expected 'Hello, World!', got '$output'"
    exit 1
  fi
}

# テスト実行
test_say_hello
```

実行結果:
```Bash
$ bash test_example.sh
Test passed!
```

## Deep Dive (深掘り情報)
### 歴史的背景
Bashでのテストは歴史的には単純なスクリプトから開始しましたが、現在ではBats, shUnit2のようなテストフレームワークが存在します。

### 代替案
Bashに限らず、PythonのpytestやJavaScriptのJestなど他言語のテストフレームワークも選択肢です。

### 実装詳細
Bashのテストでは、`[ ]`や`[[ ]]`を使った条件式、`$()`でのコマンド置換が重要です。assertやexpectのような専門のテストコマンドはありませんが、終了ステータスを活用します。

## See Also (参考リンク)
- テストフレームワーク Bats: https://github.com/bats-core/bats-core
- Bash testing with shUnit2: https://github.com/kward/shunit2
- Bashスクリプトのベストプラクティス: https://www.shellcheck.net/ (ShellCheck)
