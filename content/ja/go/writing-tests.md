---
title:                "テストの作成"
date:                  2024-01-19
simple_title:         "テストの作成"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (テストとは？そしてなぜ？)
テストコードはプログラムが正しく動作することを保証する手段です。不具合を発見しやすくし、新しい機能を安心して追加できます。

## How to: (やり方)
```Go
package calculator

// Add two integers and return the result
func Add(a, b int) int {
    return a + b
}
```

テストコードを以下のように書きます：

```Go
package calculator

import "testing"

// TestAdd 関数はAdd関数をテストする
func TestAdd(t *testing.T) {
    got := Add(1, 2)
    want := 3

    if got != want {
        t.Errorf("Add(1, 2) = %d; want %d", got, want)
    }
}
```

テストを実行する：

```
$ go test
```

サンプル出力：

```
PASS
ok      calculator 0.002s
```

## Deep Dive (詳細解説)
Go言語でのテスト実践は、2009年の初版リリース当初からサポートされています。`testing` パッケージが標準で提供され、`go test` コマンドでテスト実行が可能です。代替として、ベンチマークやカスタムテストフレームワークを利用することもできますが、単純かつ高速なテストには `testing`パッケージが適しています。

## See Also (関連情報)
- [Go Testing](https://golang.org/pkg/testing/): 公式テストパッケージドキュメント
- [Learn Go with Tests](https://github.com/quii/learn-go-with-tests): テスト駆動開発でGoを学ぶリソース
- [Go Test Tutorial](https://blog.alexellis.io/golang-writing-unit-tests/): Go言語における単体テストの書き方チュートリアル
