---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:22.131298-07:00
description: "Rust\u3067\u30C6\u30B9\u30C8\u3092\u66F8\u304F\u3053\u3068\u306F\u3001\
  \u30B3\u30FC\u30C9\u304C\u671F\u5F85\u901A\u308A\u306B\u52D5\u4F5C\u3059\u308B\u3053\
  \u3068\u3092\u4FDD\u8A3C\u3059\u308B\u81EA\u52D5\u30C1\u30A7\u30C3\u30AF\u3092\u4F5C\
  \u6210\u3059\u308B\u3053\u3068\u3092\u542B\u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\u65E9\u671F\
  \u306B\u30D0\u30B0\u3092\u6355\u6349\u3057\u3001\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\
  \u30F3\u30B0\u3092\u5BB9\u6613\u306B\u3057\u3001\u6642\u9593\u304C\u7D4C\u3064\u306B\
  \u3064\u308C\u3066\u30B3\u30FC\u30C9\u306E\u54C1\u8CEA\u3092\u7DAD\u6301\u3057\u307E\
  \u3059\u3002"
lastmod: '2024-03-13T22:44:41.827602-06:00'
model: gpt-4-0125-preview
summary: "Rust\u3067\u30C6\u30B9\u30C8\u3092\u66F8\u304F\u3053\u3068\u306F\u3001\u30B3\
  \u30FC\u30C9\u304C\u671F\u5F85\u901A\u308A\u306B\u52D5\u4F5C\u3059\u308B\u3053\u3068\
  \u3092\u4FDD\u8A3C\u3059\u308B\u81EA\u52D5\u30C1\u30A7\u30C3\u30AF\u3092\u4F5C\u6210\
  \u3059\u308B\u3053\u3068\u3092\u542B\u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\u65E9\u671F\u306B\
  \u30D0\u30B0\u3092\u6355\u6349\u3057\u3001\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\
  \u30B0\u3092\u5BB9\u6613\u306B\u3057\u3001\u6642\u9593\u304C\u7D4C\u3064\u306B\u3064\
  \u308C\u3066\u30B3\u30FC\u30C9\u306E\u54C1\u8CEA\u3092\u7DAD\u6301\u3057\u307E\u3059\
  \u3002."
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
weight: 36
---

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
