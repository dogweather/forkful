---
title:                "Go: テストの作成"
programming_language: "Go"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/writing-tests.md"
---

{{< edit_this_page >}}

## なぜテストを書くのか
プログラミングでテストを書くことは、コードの品質を高め、バグを防ぐために必要不可欠です。また、テストを書くことで、コードの挙動をよりよく理解することができます。

## どのようにテストを書くのか
まずは、Goのテストファイルを作成しましょう。それから、テスト対象の関数やメソッドを定義し、テストケースを作成します。最後に、`testing`パッケージを使用して、テストを実行します。例を示します。

```Go
package main

import "testing"

func add(x int, y int) int {
  return x + y
}

func TestAdd(t *testing.T) {
  result := add(5, 10)
  if result != 15 {
    t.Errorf("add(5, 10) should return 15, but got %d", result)
  }
}
```

テストを実行するには、ターミナルで`go test`を実行します。出力は以下のようになります。

```
$ go test
PASS
```

## テストの深い掘り下げ
テストを書く際には、以下のポイントに注意しましょう。

- テストケースは可能な限り多く用意する
- エラーメッセージを分かりやすくする
- テスト対象の関数やメソッドが変更された場合は、テストも変更する
- テストカバレッジを確認する

また、テストを書くことで得られるメリットは以下の通りです。

- バグを発見しやすくなる
- コードの品質が向上する
- テストを実行することで機能を実行するよりも早くフィードバックを得ることができる

## それでは、実際にテストを書いてみましょう！

## 参考リンク
- [Testing in Go](https://golang.org/pkg/testing/)
- [Learn Go with Tests](https://quii.gitbook.io/learn-go-with-tests/)
- [Effective Go](https://golang.org/doc/effective_go.html#testing) (公式ドキュメントのテストに関するセクション)