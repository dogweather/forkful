---
title:                "Go: 「テストの書き方」"
simple_title:         "「テストの書き方」"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/writing-tests.md"
---

{{< edit_this_page >}}

# なぜテストを書く必要があるのか？

テストを書くことは、プログラムのバグを見つけるだけではありません。テストを書くことで、コードの品質を向上させ、将来的な変更や追加を行う際にもコードの保守性を高めることができます。

## テストを書く方法

テストを書く方法を説明する前に、Go言語の基本的な文法を少し確認しましょう。

例として、以下のような関数があるとします。

```Go
func multiply(x, y int) int {
    return x * y
}
```

この関数をテストするため、同じパッケージ内に`multiply_test.go`という名前のファイルを作成し、以下のようにテストコードを書きます。

```Go
func TestMultiply(t *testing.T) {
    result := multiply(2, 3)

    if result != 6 {
        t.Errorf("Expected result to be 6, but got %d", result)
    }
}
```

テスト関数は`Test`で始まる名前にすることで、テストとして認識されます。`t *testing.T`は、テスト結果を扱うためのテストヘルパーです。`multiply`関数を呼び出して結果を受け取り、期待する値と比較しています。もし結果が期待通りでない場合、エラーを出力するようになっています。

テストを実行するには、次のコマンドをターミナルで実行します。

```bash
go test
```

これでテストが実行され、`PASS`と表示されればテストが成功したことを意味します。

## テストの詳細を知る

テストの詳細に入る前に、いくつかの用語を説明します。

- テストカバレッジ：どのくらいの割合のコードがテストでカバーされているかを表す指標です。高いテストカバレッジは、より信頼性の高いコードを作るために重要です。
- テストドリブン開発（TDD）：テストを先に書くことで、コードをより堅牢にする開発手法です。
- テストダブル：テスト中に、本来のオブジェクトや機能の代わりに使用するダミーオブジェクトやモックオブジェクトのことを指します。

これらの用語を覚えておくと、テストを書く際により効果的に活用することができます。

テストを書く際には、いくつかのテストケースを考え、それに対する期待する結果を明確に定義することが大切です。また、テストの実行にかかる時間を短くするために、テストダブルを使用すると良いでしょう。

## See Also

- [Go言語によるテスト駆動開発 - Qiita](https://qiita.com/masakinihirota/items/cb3fc95e7e0a74cc4a72)
- [Goのテストの書き方について - Qiita](https://qiita.com/masakinihirota/items/9751842adad69d3533b8)
- [Go言語におけるテスト駆動開発入門 - プログラマーズ雑記](https://prog-8.com/learn/go-tdd)