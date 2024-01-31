---
title:                "テストの作成"
date:                  2024-01-19
html_title:           "Bash: テストの作成"
simple_title:         "テストの作成"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (テストの書き方と理由)
テストとはコードが予期した通りに機能するか検証することです。プログラマーはバグを未然に防ぎ、機能の追加や変更が既存のコードに悪影響を与えないことを確かめるためにテストを書きます。

## How to: (やり方)
```Rust
// 1. この関数が正しい加算結果を返すことをテストする。
#[cfg(test)]
mod tests {
    #[test]
    fn add_two_test() {
        assert_eq!(4, super::add_two(2));
    }
}

// 2. 実際の関数
fn add_two(a: i32) -> i32 {
    a + 2
}

// コマンドラインでテストを実行:
// $ cargo test
```

Sample output:
```
running 1 test
test tests::add_two_test ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

## Deep Dive (詳細情報)
Rustのテストは、`#[test]`アトリビュートを用いて書かれる。テスト駆動開発(TDD)やユニットテストなどの概念が広まったのは過去数十年のこと。Rustでは、標準で`cargo test`を使ってテストを実行でき、これは言語の安全性と信頼性を担保する哲学に合致している。代替としては、結合テストやドキュメントテストなどが存在する。実装の面では、Rustのテストは並列に実行されるが、`--test-threads`フラグでスレッド数を調整することができる。

## See Also (関連情報)
- [Rust by Example: Testing](https://doc.rust-lang.org/rust-by-example/testing.html)
- [The Rust Programming Language: Writing Automated Tests](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Rust and WebAssembly: Testing](https://rustwasm.github.io/docs/book/game-of-life/testing.html)
