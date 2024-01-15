---
title:                "テストの書き方"
html_title:           "Bash: テストの書き方"
simple_title:         "テストの書き方"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/writing-tests.md"
---

{{< edit_this_page >}}

## なぜ？

プログラムを書く際に、テストを書くことは非常に重要です。テストを書くことで、コードの品質を保証し、バグの発生を未然に防ぐことができます。また、将来的な変更にも対応しやすくなります。

## どのようにするか？

テストを書くために、まずはBashのテストフレームワークである[Bats](https://github.com/bats-core/bats-core)をインストールします。次に、テストしたいコードをBashスクリプトで書きます。最後に、Batsを使用してテストを実行します。下記の例をご覧ください。

```
#! /usr/bin/env bash

# test.sh

# Batsのインストール（MacOS）
$ brew install bats

# テストケースを含んだBashスクリプト
# addition.sh

#!/usr/bin/env bats

load test_helper.bash

@test "1 + 1 は 2であること" {
  run addition 1 1
  [ "$status" -eq 0 ]
  [ "$output" -eq 2 ]
}

@test "10 + 5 は 15であること" {
  run addition 10 5
  [ "$status" -eq 0 ]
  [ "$output" -eq 15 ]
}

# テストケースの実行
$ bats addition.sh
```

上記の例では、テストを書くためにBatsのビルトイン関数である`load`や`test`などを使用しています。詳しくは、[Batsのドキュメント](https://github.com/bats-core/bats-core#usage)をご覧ください。

## ディープダイブ

テストを書く際には、以下のポイントに注意することが重要です。

- テストは可能な限り自動化することが望ましいです。
- テストはコードと同じリポジトリ内に保存することで、将来的な変更に対応しやすくなります。
- テストコードも頻繁にリファクタリングすることで、品質を保つことができます。

テストを書くことで、プログラムの品質を維持し、バグを未然に防ぐことができるだけでなく、開発プロセスを改善することもできます。

## 関連情報

- [Bats](https://github.com/bats-core/bats-core): Bashのためのテストフレームワーク
- [テスト駆動開発](https://ja.wikipedia.org/wiki/%E3%83%86%E3%82%B9%E3%83%88%E9%A7%86%E5%8B%95%E9%96%8B%E7%99%BA): テストを最初に書くための手法の一つ