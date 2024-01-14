---
title:    "Go: テストの書き方"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/go/writing-tests.md"
---

{{< edit_this_page >}}

[Japanese Title: テストを書く理由と方法、そして深掘り]

## Why
テストを書く理由は、コードの品質を向上させるためです。テストを書くことで、バグやエラーを早期に発見することができます。また、コードの変更や修正を行った際にも、テストがあれば影響範囲を把握しやすくなります。

## How To
テストを書く方法は簡単です。まず、Go言語で書かれたコードの中にある関数やメソッドを、テストするための専用の関数に変換します。例えば、以下のようなコードがあったとします。

```Go
func add(x int, y int) int {
    return x + y
}
```

この関数をテストするためには、以下のようにTest関数を作成します。

```Go
func TestAdd(t *testing.T) {
    result := add(2,3)
    expected := 5
    if result != expected {
        t.Errorf("Expected %d, but got %d", expected, result)
    }
}
```

以上のように、テスト対象の関数をTest関数内で実行し、期待する結果と実際の結果が一致するかどうかを確認することでテストが完了します。

## Deep Dive
テストを書く際には、テストケースをできるだけ網羅的に作成することが重要です。また、テストを書くことで得られる利点の一つに、コードのリファクタリングが挙げられます。テストがあることで、コードの修正後にテストが通るかどうかを確認することができ、コードの品質を向上させることができます。

## See Also
- Test-driven development (TDD)
- Writing tests in Go: https://golang.org/pkg/testing/
- How to write effective tests: https://www.guru99.com/software-testing.html