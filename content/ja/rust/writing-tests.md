---
title:    "Rust: テストを書く"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## なぜ

Rustプログラミングを行う人々にとって、テストを書くことの重要性は言うまでもありません。しかし、それでもなぜテストを書く必要があるのでしょうか？それは、テストによってコードが期待通りに動作することを確認できるからです。テストを書くことによって、バグの早期発見やコードの品質向上など、さまざまなメリットが得られます。

## 方法

まずは、実際にRustでテストを書く方法を見てみましょう。以下のコードは、2つの数値を加算するシンプルな関数をテストするものです。

```Rust
fn add(x: i32, y: i32) -> i32 {
    x + y
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(add(3, 5), 8);
    }
}
```

上記のコードは、`add`関数に対して`assert_eq`マクロを使用し、期待する結果と実際の結果が一致するかをテストしています。もしテストが失敗した場合は、エラーメッセージを通してどの部分のコードに問題があるかを特定することができます。

## ディープダイブ

テストを書く際に注意すべきポイントはいくつかあります。まず、バグを防ぐためにテストカバレッジを高めることが重要です。また、テストコードも品質を保つためにきちんとメンテナンスする必要があります。さらに、ユニットテストや統合テストなど、適切な種類のテストを選択することも重要です。

## その他のリソース

- [Rustでのテストの書き方](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [テスト駆動開発入門](https://tdd.coding-guidelines.com/rust/)
- [Rustのテスト実行時にカバレッジを取得する方法](https://blog.ymgyt.io/entry/2021/04/25/235926)

## 関連リンク

- [Rust公式ドキュメント：テスト](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Rust公式ブログ：テスト駆動開発入門](https://blog.rust-lang.org/2021/08/13/TDD-with-Rust.html)
- [テストツール：cargo test](https://doc.rust-lang.org/cargo/commands/cargo-test.html)