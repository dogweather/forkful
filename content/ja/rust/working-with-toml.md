---
title:                "TOMLを扱う方法"
aliases:
- ja/rust/working-with-toml.md
date:                  2024-01-26T04:26:18.999462-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOMLを扱う方法"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/working-with-toml.md"
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
