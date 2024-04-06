---
date: 2024-01-26 04:26:18.999462-07:00
description: "\u65B9\u6CD5\uFF1A TOML\u306F\u3001Tom's Obvious, Minimal Language\u306E\
  \u7565\u3067\u3042\u308A\u3001Tom Preston-\u2026"
lastmod: '2024-04-05T21:53:42.752838-06:00'
model: gpt-4-0125-preview
summary: "TOML\u306E\u4EE3\u66FF\u306B\u306FJSON\u3001YAML\u3001XML\u304C\u3042\u308A\
  \u307E\u3059\u304C\u3001\u975E\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306B\u3088\u308B\
  \u30D5\u30A1\u30A4\u30EB\u7DE8\u96C6\u3068\u4EBA\u9593\u306E\u53EF\u8AAD\u6027\u304C\
  \u91CD\u8981\u306A\u30B7\u30CA\u30EA\u30AA\u3067\u306FTOML\u304C\u512A\u52E2\u3067\
  \u3059\u3002Rust\u3067TOML\u3092\u6271\u3046\u5834\u5408\u3001serde\u306F\u30B7\u30EA\
  \u30A2\u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\u3068\u30C7\u30B7\u30EA\u30A2\u30E9\
  \u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\u306E\u305F\u3081\u306E\u5F37\u56FA\u306A\u57FA\
  \u76E4\u3092\u63D0\u4F9B\u3057\u3001Rust\u306E\u69CB\u9020\u4F53\u306BTOML\u3092\
  \u52AA\u529B\u306A\u304F\u30DE\u30C3\u30D4\u30F3\u30B0\u3059\u308B\u305F\u3081\u306E\
  \u30C8\u30EC\u30A4\u30C8\u3092\u4F7F\u7528\u3057\u307E\u3059."
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
weight: 39
---

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
