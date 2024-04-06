---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:15:24.046423-07:00
description: null
lastmod: '2024-04-05T21:53:42.333867-06:00'
model: gpt-4-0125-preview
summary: "`math.go`\u306B\u5B9A\u7FA9\u3055\u308C\u305F\u95A2\u6570`Add`\u306E\u7C21\
  \u5358\u306A\u30C6\u30B9\u30C8\u306E\u4F8B\uFF1A."
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
weight: 36
---

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
