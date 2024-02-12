---
title:                "テストの作成"
aliases:
- ja/rust/writing-tests.md
date:                  2024-02-03T19:32:22.131298-07:00
model:                 gpt-4-0125-preview
simple_title:         "テストの作成"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何を、なぜ？

Rustでテストを書くことは、コードが期待通りに動作することを保証する自動チェックを作成することを含みます。プログラマーはこれを行うことで早期にバグを捕捉し、リファクタリングを容易にし、時間が経つにつれてコードの品質を維持します。

## どうやって：

Rustの組み込みテストフレームワークは、ユニットテスト、統合テスト、ドキュメントテストを外部ライブラリなしでサポートしています。テストは `#[test]` で注釈され、そのように注釈された任意の関数はテストとしてコンパイルされます。

### ユニットテストを書く：

`#[cfg(test)]` でマークされた `tests` サブモジュールを使用して、テストしているモジュール内にユニットテストを配置します。これにより、テスト時にのみコンパイルされることが保証されます。

```rust
// lib.rs もしくは main.rs
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_adds_two() {
        assert_eq!(add(2, 2), 4);
    }
}
```

テストの実行：
```shell
$ cargo test
```

出力：
```shell
   Compiling your_package_name v0.1.0 (/path/to/your_package)
    Finished test [unoptimized + debuginfo] target(s) in 0.00 secs
     Running unittests src/lib.rs (または src/main.rs)

1つのテストを実行中
test tests::it_adds_two ... ok

テスト結果: ok。1つ通過；0つ失敗；0つ無視；0つ測定済み；0つフィルター外
```

### 統合テストを書く：

統合テストは、プロジェクトのトップレベルにある `src` の隣の tests ディレクトリに入ります。`tests` 内の各 `.rs` ファイルは、それぞれ別のクレートとしてコンパイルされます。

```rust
// tests/integration_test.rs
use your_package_name;

#[test]
fn it_adds_two() {
    assert_eq!(your_package_name::add(2, 2), 4);
}
```

### 人気のあるサードパーティライブラリを使ったテスト：

より広範なテスト機能については、`proptest` ライブラリは関数をテストするための幅広い入力を生成することができます。

`Cargo.toml` に `proptest` を開発依存関係として追加する：

```toml
[dev-dependencies]
proptest = "1.0"
```

自動的に生成された多くの入力で同じテストを実行するために `proptest` を使用する：

```rust
// tests/integration_test.rs もしくはモジュールの #[cfg(test)] の内部で

use proptest::prelude::*;

proptest! {
    #[test]
    fn doesnt_crash(a: i32, b:i32) {
        your_package_name::add(a, b);
    }
}
```

これは `add` が幅広い `i32` の入力に対してパニックしないことをチェックします。
