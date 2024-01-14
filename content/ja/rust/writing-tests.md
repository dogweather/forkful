---
title:                "Rust: テストの書き方"
simple_title:         "テストの書き方"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/writing-tests.md"
---

{{< edit_this_page >}}

## なぜ

Rustでテストを書くことの利点は、プログラミングの新しい言語を学ぶことが好きな方にとっては、Rustの文法や機能を実際に使って学ぶことができるという点にあります。また、テストを書くことで、自分のコードが予想通りの動きをするかどうかを確認できるため、より安心してコードを書くことができます。

## テストの書き方

以下のように、```Rust ... ```のコードブロックを使用して、テストを書く方法を説明します。テスト関数は、通常は"test"という接頭辞を持ちます。また、assertマクロを使用して、テスト結果をチェックすることができます。

```Rust
// テスト関数の例
#[test]
fn test_addition() {
  let result = add(2, 3);
  assert!(result == 5);
}
```

上記の例では、add関数に引数2と3を渡し、その結果が5であることをチェックしています。テストがパスすると、コンソールに`test test_addition ... ok`と表示されます。

## テストの詳細

テストを書く際には、以下の点に注意することが重要です。

- テストは、トップレベルの関数として定義する必要があります。
- テストファイルの先頭には、``` #[cfg(test)] ```という属性を追加する必要があります。
- テスト関数は、通常は#[test]という属性を持ちます。
- assertマクロの他にも、assert_eqやassert_neなど、様々なアサーションマクロがあります。自分のテストに合わせて適切なものを使用しましょう。

さらに詳しくテストについて学びたい方は、以下のリンクを参考にしてください。

## 参考リンク

- [Rust公式ドキュメント - テスト](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Rust by Example - Testing](https://doc.rust-lang.org/stable/rust-by-example/testing.html)
- [Rustlings - テストを書く](https://github.com/rust-lang/rustlings/blob/main/docs/exercises/tests.md)

## その他

この記事を読んでみて、Rustでテストを書くことがどれだけ簡単で有益かわかっていただけたでしょうか。ぜひ実際にコードを書いて、自分のスキルを向上させてみてください。

見て見ぬふりをしていると、エラーを発見するのに時間がかかってしまうかもしれません。しかし、テストを書くことで、エラーを早期に発見し、修正することができます。これにより、より良いコードを書くことができるようになります。

それでは、Rustでテストを楽しんでください！

## 参考リンク

- [Rust公式ドキュメント - テスト](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Rust by Example - Testing](https://doc.rust-lang.org/stable/rust-by-example/testing.html)
- [Rustlings - テストを書く](https://github.com/rust-lang/rustlings/blob/main/docs/exercises/tests.md)