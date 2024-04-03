---
date: 2024-01-20 18:04:41.464737-07:00
description: "How to: (\u65B9\u6CD5) Rust\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\
  \u59CB\u3081\u308B\u306B\u306F\u3001Cargo\uFF08Rust\u306E\u30D1\u30C3\u30B1\u30FC\
  \u30B8\u30DE\u30CD\u30FC\u30B8\u30E3\u30FC\uFF09\u3092\u5229\u7528\u3057\u307E\u3059\
  \u3002\u4EE5\u4E0B\u306E\u30B9\u30C6\u30C3\u30D7\u3067\u65B0\u3057\u3044\u30D7\u30ED\
  \u30B8\u30A7\u30AF\u30C8\u3092\u4F5C\u6210\u3057\u307E\u3057\u3087\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.822893-06:00'
model: gpt-4-1106-preview
summary: "Rust\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u59CB\u3081\u308B\u306B\u306F\
  \u3001Cargo\uFF08Rust\u306E\u30D1\u30C3\u30B1\u30FC\u30B8\u30DE\u30CD\u30FC\u30B8\
  \u30E3\u30FC\uFF09\u3092\u5229\u7528\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306E\u30B9\
  \u30C6\u30C3\u30D7\u3067\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\
  \u4F5C\u6210\u3057\u307E\u3057\u3087\u3046."
title: "\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u59CB\u3081\u308B"
weight: 1
---

## How to: (方法)
Rustプロジェクトを始めるには、Cargo（Rustのパッケージマネージャー）を利用します。以下のステップで新しいプロジェクトを作成しましょう。

```Rust
// コマンドラインで実行
cargo new my_project
cd my_project
```

これでディレクトリ`my_project`が作成され、`Cargo.toml`などの基本的なファイルがセットアップされます。`src/main.rs`に「Hello, world!」プログラムがすでに書かれています。

実行してみましょう。
```Rust
// コマンドラインで実行
cargo run
```

出力:
```
   Compiling my_project v0.1.0 (/path/to/my_project)
    Finished dev [unoptimized + debuginfo] target(s) in 0.5 secs
     Running `target/debug/my_project`
Hello, world!
```

## Deep Dive (深掘り)
Rustで新しいプロジェクトを開始するには`cargo new`コマンドを使います。CargoはRustのビルドシステムであり、パッケージマネージャーも兼ねています。2000年代初頭にRustが登場してから、CargoはRustエコシステムにおける中心的な存在となりました。

代替手段としては、直接ソースファイルをセットアップする方法もありますが、Cargoを利用する方が、依存関係の管理やビルドプロセスが簡単になります。実際、Cargoはプロジェクトの成長に合わせて拡大できるので、小さなプロジェクトから大きなプロジェクトまで、幅広いニーズに対応しています。

Cargoのさらなる詳細について知るには、公式ドキュメントを参考にするのが良いでしょう。これには、依存性管理、Cratesの公開方法、カスタムビルドスクリプトの書き方などが含まれています。

## See Also (関連情報)
- Rustの公式ガイド: https://www.rust-lang.org/learn
- Cargoのドキュメント: https://doc.rust-lang.org/cargo/
- Rustのコミュニティフォーラム: https://users.rust-lang.org/
