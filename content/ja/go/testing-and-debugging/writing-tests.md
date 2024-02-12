---
title:                "テストの作成"
date:                  2024-02-03T18:15:24.046423-07:00
model:                 gpt-4-0125-preview
simple_title:         "テストの作成"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何を、なぜ？

Goでテストを書くというのは、アプリケーションの機能や振る舞いを検証する小さく管理しやすいコードの断片を作成することを意味します。プログラマーは、さまざまな条件下でコードが期待通りに動作することを確認し、リファクタリングを容易にし、リグレッションを防ぐためにテストを書きます。

## どのように：

Goでは、テストは一般的にテストしているコードと同じパッケージ内に書かれます。テストを含むファイルは、`_test.go`のサフィックスで名付けられます。テストは、`testing`パッケージからのtesting.Tオブジェクトへのポインタを引数として取る関数であり、`t.Fail()`、`t.Errorf()`などのメソッドを呼び出して失敗を通知します。

`math.go`に定義された関数`Add`の簡単なテストの例：
```go
// math.go
package math

func Add(x, y int) int {
    return x + y
}
```

テストファイル`math_test.go`：
```go
package math

import "testing"

func TestAdd(t *testing.T) {
    result := Add(1, 2)
    expected := 3
    if result != expected {
        t.Errorf("Add(1, 2) = %d; want %d", result, expected)
    }
}
```

テストファイルと同じディレクトリで`go test`コマンドを使用してテストを実行します。テストが通過したことを示すサンプル出力は以下のようになります：

```
PASS
ok      example.com/my/math 0.002s
```

さまざまな入力と出力の組み合わせを効率的にテストするためのテーブル駆動テストでは、テストケースを表す構造体のスライスを定義します：

```go
func TestAddTableDriven(t *testing.T) {
    var tests = []struct {
        x        int
        y        int
        expected int
    }{
        {1, 2, 3},
        {2, 3, 5},
        {-1, -2, -3},
    }

    for _, tt := range tests {
        testname := fmt.Sprintf("%d+%d", tt.x, tt.y)
        t.Run(testname, func(t *testing.T) {
            ans := Add(tt.x, tt.y)
            if ans != tt.expected {
                t.Errorf("got %d, want %d", ans, tt.expected)
            }
        })
    }
}
```

## 深掘り

Go自体と同時に導入されたGoのテストフレームワークは、ソフトウェア開発におけるシンプルさと効率性を重視するGoの強調点を反映して、Goツールチェーンとシームレスに統合されるように設計されました。他の言語の一部のテストフレームワークが外部ライブラリや複雑な設定に依存するのとは異なり、Goの組み込み`testing`パッケージは、テストの書き方と実行方法を直接的に提供します。

Goのテストへのアプローチの興味深い側面は、ファイル命名パターン（`_test.go`）や外部依存関係よりも標準ライブラリの機能の使用など、採用されている規約よりも構成原則です。このミニマリスティックなアプローチは開発者がテストを書くことを奨励します、なぜなら入門のハードルが低いからです。

Goの組み込みテスト機能が多くをカバーしているものの、モック生成、ファズテスティング、またはBDD（ビヘイビア駆動開発）スタイルのテストなど、より多くの機能を提供する場合があるサードパーティのツールやフレームワークがあります。TestifyやGoMockのような人気ライブラリは、Goの標準テスティング機能を補完し、より表現力豊かなアサーションやモック生成機能を提供します。これらは、多くの依存関係を持つ複雑なアプリケーションでは特に有用です。

これらの代替品が存在するにもかかわらず、そのシンプルさ、パフォーマンス、および言語およびツールチェーンとの密接な統合のため、標準のGoテストパッケージはGoでのテストの基礎として残ります。開発者がサードパーティツールでそれを強化するかどうかにかかわらず、Goのテストフレームワークは、コード品質と信頼性を保証するための確かな基盤を提供します。
