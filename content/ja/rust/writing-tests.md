---
title:    "Rust: テストの書き方"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/rust/writing-tests.md"
---

{{< edit_this_page >}}

##なぜテストを書くのか
プログラミングにおいて、私たちは常に正しいコードを書くことを目指しています。しかし、実際の開発ではバグが発生することもあります。テストを書くことで、コードの正しさを確認し、バグを発見しやすくすることができます。さらに、テストを書くことでコードの品質が向上し、今後の変更や追加に対しても安心してコードを変更することができます。

##テストの書き方
テストを書くには、以下のような手順を踏みます。

1. テスト用のRustプロジェクトを作成する
```Rust
cargo new myproject --lib
```

2. `src/lib.rs`ファイルにテストしたいコードを記述する
```Rust
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(add(2, 3), 5);
    }
}
```
 `add`関数を`pub`で公開し、テスト用の`tests`モジュール内でテストを定義します。`assert_eq`マクロを使うことで、テストが成功したかどうかを確認します。

3. テストを実行する
```Rust
cargo test
```
ターミナルには以下のような出力が表示されるでしょう。
```text
running 1 test
test tests::test_add ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

##テストの深掘り
テストを書く際には、テストカバレッジやTDD（Test Driven Development）など、さまざまなテスト手法があります。また、`#[should_panic]`や`#[ignore]`といったアトリビュートを使用することで、より複雑なテストケースを作成することもできます。

しかし、テストを書くにあたって最も大切なのは、コードが読みやすいことです。テストを読めば、そのコードがどのような動作をするかがわかるようにすることが重要です。また、テストを書くことで多くのバグを未然に防ぐことができます。テストはプログラムの信頼性を高めるために欠かせないものです。

##See Also
- [Official Rust Testing Documentation](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Rust Test Tutorial: Getting Started with Writing Tests](https://www.youtube.com/watch?v=aHyxqJdY4zo)
- [The Red, Green, Refactor Cycle of Test Driven Development](https://medium.com/@amlcurran/the-red-green-refactor-cycle-of-test-driven-development-ee98d973b4d9)