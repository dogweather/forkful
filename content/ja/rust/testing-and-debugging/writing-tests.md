---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:22.131298-07:00
description: "\u3069\u3046\u3084\u3063\u3066\uFF1A Rust\u306E\u7D44\u307F\u8FBC\u307F\
  \u30C6\u30B9\u30C8\u30D5\u30EC\u30FC\u30E0\u30EF\u30FC\u30AF\u306F\u3001\u30E6\u30CB\
  \u30C3\u30C8\u30C6\u30B9\u30C8\u3001\u7D71\u5408\u30C6\u30B9\u30C8\u3001\u30C9\u30AD\
  \u30E5\u30E1\u30F3\u30C8\u30C6\u30B9\u30C8\u3092\u5916\u90E8\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u306A\u3057\u3067\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u3044\u307E\u3059\u3002\
  \u30C6\u30B9\u30C8\u306F `#[test]` \u3067\u6CE8\u91C8\u3055\u308C\u3001\u305D\u306E\
  \u3088\u3046\u306B\u6CE8\u91C8\u3055\u308C\u305F\u4EFB\u610F\u306E\u95A2\u6570\u306F\
  \u30C6\u30B9\u30C8\u3068\u3057\u3066\u30B3\u30F3\u30D1\u30A4\u30EB\u3055\u308C\u307E\
  \u3059\u3002 `#[cfg(test)]` \u3067\u30DE\u30FC\u30AF\u3055\u308C\u305F\u2026"
lastmod: '2024-03-13T22:44:41.827602-06:00'
model: gpt-4-0125-preview
summary: "Rust\u306E\u7D44\u307F\u8FBC\u307F\u30C6\u30B9\u30C8\u30D5\u30EC\u30FC\u30E0\
  \u30EF\u30FC\u30AF\u306F\u3001\u30E6\u30CB\u30C3\u30C8\u30C6\u30B9\u30C8\u3001\u7D71\
  \u5408\u30C6\u30B9\u30C8\u3001\u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\u30C6\u30B9\u30C8\
  \u3092\u5916\u90E8\u30E9\u30A4\u30D6\u30E9\u30EA\u306A\u3057\u3067\u30B5\u30DD\u30FC\
  \u30C8\u3057\u3066\u3044\u307E\u3059\u3002\u30C6\u30B9\u30C8\u306F `#[test]` \u3067\
  \u6CE8\u91C8\u3055\u308C\u3001\u305D\u306E\u3088\u3046\u306B\u6CE8\u91C8\u3055\u308C\
  \u305F\u4EFB\u610F\u306E\u95A2\u6570\u306F\u30C6\u30B9\u30C8\u3068\u3057\u3066\u30B3\
  \u30F3\u30D1\u30A4\u30EB\u3055\u308C\u307E\u3059."
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
weight: 36
---

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
