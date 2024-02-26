---
date: 2024-01-26 04:26:18.999462-07:00
description: "TOML\u306F\u3001\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u306B\u3088\u304F\
  \u4F7F\u7528\u3055\u308C\u308B\u3001\u4EBA\u9593\u304C\u8AAD\u3080\u3053\u3068\u304C\
  \u3067\u304D\u308B\u30C7\u30FC\u30BF\u30B7\u30EA\u30A2\u30E9\u30A4\u30BC\u30FC\u30B7\
  \u30E7\u30F3\u8A00\u8A9E\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u305D\u306E\u30B7\u30F3\u30D7\u30EB\u3055\u3068\u660E\u77AD\u3055\u304B\u3089TOML\u3092\
  \u4F7F\u7528\u3057\u3001Rust\u306E\u30CF\u30C3\u30B7\u30E5\u30DE\u30C3\u30D7\u306B\
  \u5BB9\u6613\u306B\u5909\u63DB\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\
  \u3002"
lastmod: '2024-02-25T18:49:39.901438-07:00'
model: gpt-4-0125-preview
summary: "TOML\u306F\u3001\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u306B\u3088\u304F\u4F7F\
  \u7528\u3055\u308C\u308B\u3001\u4EBA\u9593\u304C\u8AAD\u3080\u3053\u3068\u304C\u3067\
  \u304D\u308B\u30C7\u30FC\u30BF\u30B7\u30EA\u30A2\u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\
  \u30F3\u8A00\u8A9E\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u305D\
  \u306E\u30B7\u30F3\u30D7\u30EB\u3055\u3068\u660E\u77AD\u3055\u304B\u3089TOML\u3092\
  \u4F7F\u7528\u3057\u3001Rust\u306E\u30CF\u30C3\u30B7\u30E5\u30DE\u30C3\u30D7\u306B\
  \u5BB9\u6613\u306B\u5909\u63DB\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\
  \u3002"
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
---

{{< edit_this_page >}}

## 何となぜ？
TOMLは、設定ファイルによく使用される、人間が読むことができるデータシリアライゼーション言語です。プログラマーはそのシンプルさと明瞭さからTOMLを使用し、Rustのハッシュマップに容易に変換することができます。

## 方法：
```Rust
// 1. Cargo.tomlに'toml'クレートを含める
// [依存性]
// toml = "0.5"

// 2. TOMLをRustの構造体にデシリアライズする
use toml::Value;

fn main() {
    let toml_content = r#"
        [server]
        host = "localhost"
        port = 8080
    "#;

    let value = toml_content.parse::<Value>().unwrap();
    let host = value.get("server").unwrap().get("host").unwrap();
    let port = value.get("server").unwrap().get("port").unwrap();
    
    println!("The server is running on {}:{}", host, port);
    // 出力：The server is running on "localhost":8080
}
```

## 詳細解説
TOMLは、Tom's Obvious, Minimal Languageの略であり、Tom Preston-Wernerによって2013年に作られました。それは、設定ファイルにおいてJSONやYAMLよりも読みやすいことを目指しています。TOMLの設計は、曖昧さのない構文、最小主義、そしてデータ型への容易なマッピングに焦点を当てています。

TOMLの代替にはJSON、YAML、XMLがありますが、非プログラマーによるファイル編集と人間の可読性が重要なシナリオではTOMLが優勢です。RustでTOMLを扱う場合、serdeはシリアライゼーションとデシリアライゼーションのための強固な基盤を提供し、Rustの構造体にTOMLを努力なくマッピングするためのトレイトを使用します。

TOMLをRustで効果的に利用するためには、TOMLデータのスキーマを反映した整ったRustタイプシステムをプログラマーが定義する必要があります。これがTOMLを扱う際の挑戦の一つです。

## 参照
- [TOMLドキュメンテーション](https://toml.io/en/)
- [serde_tomlクレート](https://docs.rs/serde_toml/)
- [Rustプログラミング言語の書籍](https://doc.rust-lang.org/stable/book/)
- [TOML GitHubリポジトリ](https://github.com/toml-lang/toml)
